library(shiny)
library(DT)
library(tidyverse)
library(readxl)
library(plotly)
library(shinycssloaders)
library(shinyjs)
library(filelock)

# Define experimental sections and measures based on protocol
experiment_sections <- c("Pre-Quiz", "Post-Quiz", "Baseline", "City Sounds and Images", 
                         "Nature Sounds and Images", "Cognitive Test", "Guided Meditation")

section_prefixes <- c("prequiz", "postquiz", "baseline", "city", "nature", "cognitive", "med")

affect_measures <- c(
  stress = "Rate your overall level of stress",
  neck = "Rate the tension in your neck/shoulders", 
  chest = "Rate the tension in your chest/stomach",
  sweat = "Rate your sweatiness",
  breath = "Rate the elevation of your heartrate/breathing",
  mouth = "Rate the dryness of your mouth"
)

# Optional documents for the home page. A download button is shown for each
# file that is present in the docs/ folder.
protocol_file <- file.path("docs", "protocol.docx")
guide_file <- file.path("docs", "facilitators_guide.docx")

# Function to generate unique project key
generate_project_key <- function() {
  chars <- c(letters, LETTERS, 0:9)
  paste0(sample(chars, 8, replace = TRUE), collapse = "")
}

# Function to clean column names; netid becomes id
clean_column_names <- function(names) {
  names <- trimws(names)
  names <- gsub("[^a-zA-Z0-9_]", "", names)
  names <- tolower(names)
  # Convert netid to id for consistency
  names <- gsub("^netid$", "id", names)
  return(names)
}

# Function to detect and remove header rows
detect_header_rows <- function(df) {
  header_patterns <- c(
    "Start Date", "StartDate", "start date",
    "ImportId", "QID", "\\{.*\\}",
    "Response Type", "Response ID",
    "Please enter", "Rate your"
  )
  
  n_check <- min(10, nrow(df))
  header_rows <- c()
  
  for (i in 1:n_check) {
    row_values <- as.character(unlist(df[i, ]))
    if (any(grepl(paste(header_patterns, collapse = "|"), row_values, ignore.case = TRUE))) {
      header_rows <- c(header_rows, i)
    }
  }
  
  return(header_rows)
}

# Function to validate uploaded data; the ID column may be named id or netid
validate_uploaded_data <- function(df) {
  report <- list(
    has_errors = FALSE,
    valid_ids = 0,
    id_issues = character()
  )
  
  # Check for an ID column named id or netid
  id_cols <- grep("^(id|netid)$", names(df), value = TRUE, ignore.case = TRUE)
  if (length(id_cols) == 0) {
    report$id_issues <- "ID column not found!"
    report$has_errors <- TRUE
  } else {
    # Use the first matching column and rename to 'id'
    if (tolower(id_cols[1]) != "id") {
      names(df)[tolower(names(df)) == tolower(id_cols[1])] <- "id"
    }
    
    df$id <- trimws(as.character(df$id))
    
    empty_ids <- which(is.na(df$id) | df$id == "")
    if(length(empty_ids) > 0) {
      report$id_issues <- c(report$id_issues, 
                            sprintf("%d rows with empty ID", length(empty_ids)))
      report$has_errors <- TRUE
    }
    
    report$valid_ids <- length(unique(df$id[!is.na(df$id) & df$id != "" & !grepl("\\{|\\}|Import|QID", df$id)]))
  }
  
  return(report)
}

# Function to safely convert to numeric
safe_numeric_conversion <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("^NA$|^N/A$|^na$|^n/a$|^\\.$|^$", NA, x)
  result <- suppressWarnings(as.numeric(x))
  return(result)
}

# Columns that hold the measurements of the protocol: a pulse count and six
# ratings for each section, and the cognitive test score.
protocol_columns <- function() {
  columns <- character()
  for (prefix in section_prefixes) {
    columns <- c(columns, paste0(prefix, "_hr30"), paste0(prefix, "_", names(affect_measures)))
  }
  c(columns, "cognitive_gamescore")
}

# Reduces an uploaded table to the participant ID and the protocol columns.
# A survey export also carries IP addresses, names, e-mail addresses and
# coordinates; none of these is stored. A protocol column that the file lacks
# is added as missing, so an uploaded record has the same columns as a record
# entered by hand.
keep_protocol_columns <- function(df) {
  kept <- data.frame(id = trimws(as.character(df$id)), stringsAsFactors = FALSE)
  for (col in protocol_columns()) {
    kept[[col]] <- if (col %in% names(df)) safe_numeric_conversion(df[[col]]) else NA_real_
  }
  kept
}

# Row numbers for `n` new records. They continue from the highest number in
# use, so a number is not given out again after a record was deleted.
next_row_ids <- function(data, n = 1) {
  highest <- if (nrow(data) > 0) max(data$row_id) else 0
  highest + seq_len(n)
}

# Writes the data table of a project, with the two files derived from it:
# the record of which submitter keys have been used, and a CSV copy.
write_project_data <- function(creator_key, data, submitter_keys) {
  project_path <- file.path("projects", creator_key)
  saveRDS(data, file.path(project_path, "data.rds"))
  
  submitter_usage <- list()
  if (nrow(data) > 0 && "submitter_key" %in% names(data)) {
    for (key in submitter_keys) {
      submitter_usage[[key]] <- any(data$submitter_key == key, na.rm = TRUE)
    }
  }
  saveRDS(submitter_usage, file.path(project_path, "submitter_usage.rds"))
  
  # The copy is rewritten even when no record is left, so that it never
  # holds records that were deleted.
  if (ncol(data) > 0) {
    write.csv(data, file.path(project_path, "data_backup.csv"), row.names = FALSE)
  }
}

# Reads the data table of a project from disk.
read_project_data <- function(creator_key) {
  data <- readRDS(file.path("projects", creator_key, "data.rds"))
  if (is.null(data)) data.frame() else data
}

# Changes the data table of a project on disk.
#
# Several people use a project at once, and each session holds its own copy
# of the table. Writing such a copy back would erase the records that other
# sessions saved in the meantime. So every change goes through this function:
# it takes a file lock, reads the table as it is on disk now, applies `change`
# to it and writes the result. `change` gets the current table and returns the
# new one, or NULL to leave the file as it is.
#
# Returns the table as it is on disk after the call.
update_project_data <- function(creator_key, change) {
  project_path <- file.path("projects", creator_key)
  lock <- filelock::lock(file.path(project_path, "data.lock"), timeout = 10000)
  if (is.null(lock)) {
    stop("the project is being saved by someone else, please try again")
  }
  on.exit(filelock::unlock(lock))
  
  current <- read_project_data(creator_key)
  updated <- change(current)
  if (is.null(updated)) {
    return(current)
  }
  
  submitter_keys <- readRDS(file.path(project_path, "metadata.rds"))$submitter_keys
  write_project_data(creator_key, updated, submitter_keys)
  updated
}

# Adds records to a project. If one of the IDs is already in the project,
# nothing is added and the IDs in question are returned in `duplicates`.
add_records <- function(creator_key, records) {
  duplicates <- character()
  data <- update_project_data(creator_key, function(current) {
    duplicates <<- unique(records$id[tolower(records$id) %in% tolower(current$id)])
    if (length(duplicates) > 0) {
      return(NULL)
    }
    records$row_id <- next_row_ids(current, nrow(records))
    rbind(current, records)
  })
  list(added = length(duplicates) == 0, duplicates = duplicates, data = data)
}

# Deletes one record from a project.
delete_record <- function(creator_key, row_id) {
  update_project_data(creator_key, function(current) {
    current[current$row_id != row_id, , drop = FALSE]
  })
}

# Function to save project, with a record of which submitter keys were used
save_project <- function(project_name, creator_key, submitter_keys, data, project_info = NULL) {
  if (!dir.exists("projects")) {
    dir.create("projects")
  }
  
  project_path <- file.path("projects", creator_key)
  if (!dir.exists(project_path)) {
    dir.create(project_path, recursive = TRUE)
  }
  
  metadata <- list(
    name = project_name,
    project_name = project_name,
    creator_key = creator_key,
    submitter_keys = submitter_keys,
    created_date = Sys.time(),
    last_modified = Sys.time()
  )
  
  tryCatch({
    saveRDS(metadata, file.path(project_path, "metadata.rds"))
    
    if (!is.null(project_info)) {
      saveRDS(project_info, file.path(project_path, "project_info.rds"))
    }
    
    if (is.null(data)) {
      data <- data.frame()
    }
    write_project_data(creator_key, data, submitter_keys)
    
    return(TRUE)
  }, error = function(e) {
    showNotification(paste("Error saving project:", e$message), type = "error")
    return(FALSE)
  })
}

# Function to load a project by creator key or by submitter key
load_project <- function(key) {
  # First try as creator key
  project_path <- file.path("projects", key)
  
  if (dir.exists(project_path)) {
    # This is a creator key
    tryCatch({
      metadata <- readRDS(file.path(project_path, "metadata.rds"))
      data <- readRDS(file.path(project_path, "data.rds"))
      
      project_info <- NULL
      info_file <- file.path(project_path, "project_info.rds")
      if (file.exists(info_file)) {
        project_info <- readRDS(info_file)
      }
      
      submitter_usage <- NULL
      usage_file <- file.path(project_path, "submitter_usage.rds")
      if (file.exists(usage_file)) {
        submitter_usage <- readRDS(usage_file)
      }
      
      metadata$last_accessed <- Sys.time()
      
      if (is.null(data) || (is.data.frame(data) && nrow(data) == 0)) {
        csv_file <- file.path(project_path, "data_backup.csv")
        if (file.exists(csv_file)) {
          data <- read.csv(csv_file, stringsAsFactors = FALSE)
        }
      }
      
      return(list(
        metadata = metadata, 
        data = data,
        access_type = "creator",
        project_info = project_info,
        submitter_usage = submitter_usage
      ))
    }, error = function(e) {
      print(paste("Error loading project:", e$message))
      return(NULL)
    })
  }
  
  # If not a creator key, search all projects for submitter key
  if (dir.exists("projects")) {
    all_projects <- list.dirs("projects", full.names = FALSE, recursive = FALSE)
    
    for (proj_key in all_projects) {
      metadata_file <- file.path("projects", proj_key, "metadata.rds")
      if (file.exists(metadata_file)) {
        metadata <- readRDS(metadata_file)
        
        # Check if this is a submitter key
        if (!is.null(metadata$submitter_keys) && key %in% metadata$submitter_keys) {
          data_file <- file.path("projects", proj_key, "data.rds")
          data <- if (file.exists(data_file)) readRDS(data_file) else data.frame()
          
          project_info <- NULL
          info_file <- file.path("projects", proj_key, "project_info.rds")
          if (file.exists(info_file)) {
            project_info <- readRDS(info_file)
          }
          
          # Check if this submitter key has been used
          has_submitted <- FALSE
          if (!is.null(data) && nrow(data) > 0 && "submitter_key" %in% names(data)) {
            has_submitted <- any(data$submitter_key == key, na.rm = TRUE)
          }
          
          return(list(
            metadata = metadata,
            data = data,
            access_type = "submitter",
            submitter_key = key,
            has_submitted = has_submitted,
            project_info = project_info
          ))
        }
      }
    }
  }
  
  return(NULL)
}

# Table of saved records for the creator. Only the Actions column holds HTML;
# the other columns are escaped, so a participant ID is shown as it was typed.
uploaded_data_table <- function(data, can_delete) {
  display_data <- data %>%
    select(row_id, id, timestamp) %>%
    mutate(
      Actions = if (can_delete) {
        sprintf('<button onclick="Shiny.setInputValue(\'deleteRow\', %d, {priority: \'event\'})">Delete</button>',
                row_id)
      } else {
        "N/A"
      }
    )

  datatable(display_data,
            escape = c("row_id", "id", "timestamp"),
            options = list(pageLength = 10))
}

# Custom CSS
custom_css <- "
  :root {
    --dartmouth-green: #00693e;
    --forest-green: #12312b;
    --light-green: #e8f4ed;
    --accent-green: #4a9b6f;
  }
  
  body {
    font-family: 'Open Sans', 'Helvetica Neue', Arial, sans-serif;
    background-color: #f8f9fa;
  }
  
  .app-header {
    background: white;
    padding: 15px 30px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    position: fixed;
    top: 0;
    left: 0;
    right: 0;
    z-index: 1000;
    display: flex;
    align-items: center;
    justify-content: space-between;
  }
  
  .app-content {
    margin-top: 100px;
    padding: 20px;
  }
  
  .project-card {
    background: white;
    border-radius: 12px;
    padding: 40px;
    margin: 20px;
    box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
    transition: all 0.3s ease;
    cursor: pointer;
    border: 2px solid transparent;
    text-align: center;
  }
  
  .project-card:hover {
    transform: translateY(-5px);
    box-shadow: 0 8px 15px rgba(0, 0, 0, 0.15);
    border-color: var(--dartmouth-green);
  }
  
  .project-card h3 {
    color: var(--dartmouth-green);
    margin-bottom: 20px;
    font-size: 24px;
  }
  
  .project-card .icon {
    font-size: 48px;
    color: var(--accent-green);
    margin-bottom: 20px;
  }
  
  .btn-dartmouth {
    background-color: var(--dartmouth-green);
    color: white;
    border: none;
    padding: 12px 30px;
    font-size: 16px;
    border-radius: 5px;
    transition: all 0.3s ease;
  }
  
  .btn-dartmouth:hover {
    background-color: var(--forest-green);
    color: white;
  }
  
  .project-bar {
    background: var(--light-green);
    border: 1px solid var(--dartmouth-green);
    padding: 15px 30px;
    margin-bottom: 30px;
    border-radius: 8px;
    display: flex;
    align-items: center;
    justify-content: space-between;
  }
  
  .section-container {
    background: white;
    padding: 30px;
    margin-bottom: 30px;
    border-radius: 10px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }
  
  .section-header {
    color: var(--dartmouth-green);
    border-bottom: 2px solid var(--light-green);
    padding-bottom: 15px;
    margin-bottom: 25px;
  }
  
  .key-display {
    background-color: #f0f0f0;
    padding: 15px;
    border-radius: 5px;
    font-family: 'Courier New', monospace;
    font-size: 18px;
    text-align: center;
    letter-spacing: 2px;
    border: 2px dashed var(--dartmouth-green);
    margin: 20px 0;
  }
  
  .phase-box {
    margin-bottom: 15px;
    padding: 15px;
    background-color: #ffffff;
    border-radius: 8px;
    border: 1px solid #e0e0e0;
  }
  
  .phase-box h5 {
    color: var(--dartmouth-green);
    margin-bottom: 15px;
    font-weight: 600;
  }
  
  .welcome-section {
    background: linear-gradient(135deg, var(--dartmouth-green) 0%, var(--forest-green) 100%);
    color: white;
    padding: 40px;
    border-radius: 10px;
    margin-bottom: 30px;
    text-align: center;
  }
  
  .protocol-summary {
    background: white;
    padding: 30px;
    margin: 20px 0;
    border-radius: 10px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }
  
  .protocol-section {
    background: #f8f9fa;
    border-left: 4px solid var(--dartmouth-green);
    padding: 15px;
    margin: 15px 0;
  }
  
  .submitter-keys-display {
    background-color: #f8f9fa;
    padding: 20px;
    border-radius: 8px;
    margin-top: 20px;
    max-height: 300px;
    overflow-y: auto;
  }
  
  .submitter-key-item {
    background-color: white;
    padding: 10px 15px;
    margin: 5px 0;
    border-radius: 5px;
    border: 1px solid #e0e0e0;
    font-family: 'Courier New', monospace;
    display: flex;
    justify-content: space-between;
    align-items: center;
  }
  
  .selection-card {
    border: 2px solid #e0e0e0;
    border-radius: 10px;
    padding: 30px;
    margin: 20px;
    cursor: pointer;
    transition: all 0.3s ease;
    background-color: white;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }
  
  .selection-card:hover {
    border-color: var(--dartmouth-green);
    box-shadow: 0 4px 8px rgba(0,0,0,0.15);
    transform: translateY(-2px);
  }
  
  .upload-success {
    background-color: #e8f4ed;
    border: 1px solid var(--dartmouth-green);
    border-radius: 8px;
    padding: 20px;
    margin: 20px 0;
    text-align: center;
  }
  
  .stat-notes {
    background-color: #f8f9fa;
    border-left: 4px solid var(--dartmouth-green);
    padding: 15px;
    margin-top: 20px;
    font-size: 14px;
    border-radius: 5px;
  }
  
  .stat-notes h5 {
    color: var(--dartmouth-green);
    margin-bottom: 10px;
    font-weight: 600;
  }
  
  .completion-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
    gap: 15px;
    margin: 20px 0;
  }
  
  .completion-card {
    background: white;
    border: 1px solid #e0e0e0;
    border-radius: 8px;
    padding: 15px;
    text-align: center;
    box-shadow: 0 2px 4px rgba(0,0,0,0.05);
  }
  
  .completion-percentage {
    font-size: 24px;
    font-weight: bold;
    color: var(--dartmouth-green);
    margin-bottom: 5px;
  }
  
  .completion-label {
    font-size: 12px;
    color: #666;
  }
  
  .submitter-warning {
    background-color: #fff3cd;
    border: 1px solid #ffeaa7;
    border-radius: 5px;
    padding: 15px;
    margin: 20px 0;
    color: #856404;
  }
"

# Define UI
ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$style(HTML(custom_css))
  ),
  
  # Fixed header
  div(class = "app-header",
      div(
        tags$h3("Physiological Response Lab Data Collection and Analysis Platform", 
                style = "margin: 0; color: #00693e; font-weight: 600;")
      )
  ),
  
  # Main content
  div(class = "app-content",
      # Conditional UI based on state
      uiOutput("mainUI")
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    currentState = "home",  # "home" or "workspace"
    currentProject = NULL,
    projectData = data.frame(),
    uploadMethod = NULL,
    tempKeys = NULL,
    accessType = NULL,  # "creator" or "submitter"
    submitterKey = NULL,  # Store current submitter key
    continueAddingData = FALSE  # Flag for continuous data addition
  )
  
  # Main UI renderer: single page workflow
  output$mainUI <- renderUI({
    if (values$currentState == "home") {
      # Home page UI
      tagList(
        div(class = "welcome-section",
            h1("Physiological Response Lab Data Collection and Analysis Platform")
        ),
        
        # Protocol summary
        div(class = "protocol-summary",
            h3("About This System", style = "color: #00693e; margin-bottom: 20px;"),
            p("This system facilitates data collection for understanding physiological responses to different stimuli. 
               The activity consists of five experimental sections designed to elicit various physiological and psychological states:"),
            
            div(class = "protocol-section",
                h5("Experimental Protocol"),
                tags$ol(
                  tags$li(tags$strong("Baseline (5 minutes):"), " Participants sit quietly to establish baseline physiological measures"),
                  tags$li(tags$strong("City Sounds and Images (5 minutes):"), " Exposure to recorded urban environments"),
                  tags$li(tags$strong("Nature Sounds and Images (5 minutes):"), " Exposure to recorded natural environments"),
                  tags$li(tags$strong("Cognitive Test (8 minutes):"), " Arithmetic challenge using the Optiver 80 in 8 game"),
                  tags$li(tags$strong("Guided Meditation (5 minutes):"), " Structured meditation session")
                ),
                p("Between each section, participants have 2-3 minutes to return to baseline.")
            ),
            
            div(class = "protocol-section",
                h5("Data Collection"),
                p("At the end of each section, participants:"),
                tags$ul(
                  tags$li("Self-measure heart rate by counting pulse for 30 seconds at radial or carotid artery"),
                  tags$li("Complete self-report measures on six physiological and psychological dimensions (10-point scale)")
                )
            ),
            
            # Download buttons for the protocol and the facilitator's guide
            if (file.exists(protocol_file) || file.exists(guide_file)) {
              tagList(
                br(),
                fluidRow(
                  column(6,
                         if (file.exists(protocol_file)) {
                           downloadButton("downloadProtocol", "Download Complete Protocol", 
                                          class = "btn btn-primary btn-lg btn-block")
                         }
                  ),
                  column(6,
                         if (file.exists(guide_file)) {
                           downloadButton("downloadGuide", "Download Facilitator's Guide", 
                                          class = "btn btn-info btn-lg btn-block")
                         }
                  )
                )
              )
            }
        ),
        
        fluidRow(
          column(12,
                 h2("Select an Option", 
                    style = "text-align: center; margin-bottom: 40px; color: #12312b;")
          )
        ),
        
        fluidRow(
          column(6,
                 div(class = "project-card",
                     div(class = "icon", icon("folder-open")),
                     h3("Project Recall"),
                     p("Access and continue working on existing projects"),
                     p("Enter your 8-character project key to retrieve your data"),
                     br(),
                     actionButton("openRecall", "Open Project", 
                                  class = "btn-dartmouth")
                 )
          ),
          
          column(6,
                 div(class = "project-card",
                     div(class = "icon", icon("plus-circle")),
                     h3("New Project Create"),
                     p("Start a new data collection project"),
                     p("Create a unique project with secure access keys"),
                     br(),
                     actionButton("createProject", "Create Project", 
                                  class = "btn-dartmouth")
                 )
          )
        )
      )
    } else if (values$currentState == "workspace") {
      # Workspace UI
      tagList(
        # Project bar
        div(class = "project-bar",
            div(
              h4(values$currentProject$name, style = "margin: 0;"),
              p(paste("Key:", ifelse(values$accessType == "creator", 
                                     values$currentProject$creator_key, 
                                     values$submitterKey)), 
                style = "margin: 0; font-family: monospace;"),
              p(paste("Access Type:", values$accessType), 
                style = "margin: 0; font-style: italic; color: #666;")
            ),
            div(
              actionButton("refreshProject", "Refresh Data", icon = icon("sync"), 
                           class = "btn btn-success"),
              actionButton("closeProject", "Close Project", icon = icon("times"), 
                           class = "btn btn-warning", style = "margin-left: 10px;")
            )
        ),
        
        # Conditional content based on access type
        if (values$accessType == "submitter") {
          # Submitter view: limited functionality
          tagList(
            div(class = "submitter-warning",
                icon("info-circle"), " ",
                strong("Submitter Access: "), 
                "You can only submit data once with this key. After submission, you can only view the data."
            ),
            
            # Data Upload Section
            div(class = "section-container",
                h2(class = "section-header", icon("upload"), " Data Upload"),
                uiOutput("uploadInterface")
            ),
            
            # Data Preview Section: limited view
            div(class = "section-container",
                h2(class = "section-header", icon("eye"), " Data Preview"),
                p("You can view overall project statistics but not individual data."),
                uiOutput("limitedDataPreview")
            )
          )
        } else {
          # Creator view: full functionality
          tagList(
            # Data Upload Section
            div(class = "section-container",
                h2(class = "section-header", icon("upload"), " Data Upload"),
                uiOutput("uploadInterface")
            ),
            
            # Data Preview Section
            div(class = "section-container",
                h2(class = "section-header", icon("eye"), " Data Preview"),
                uiOutput("dataPreview")
            ),
            
            # Data Analysis Section
            div(class = "section-container",
                h2(class = "section-header", icon("chart-line"), " Data Analysis"),
                uiOutput("dataAnalysis")
            ),
            
            # Report Generation Section
            div(class = "section-container",
                h2(class = "section-header", icon("file-pdf"), " Report Generation"),
                uiOutput("reportGeneration")
            )
          )
        }
      )
    }
  })
  
  # Download handlers for the two optional documents
  output$downloadProtocol <- downloadHandler(
    filename = function() {
      basename(protocol_file)
    },
    content = function(file) {
      file.copy(protocol_file, file)
    }
  )
  
  output$downloadGuide <- downloadHandler(
    filename = function() {
      basename(guide_file)
    },
    content = function(file) {
      file.copy(guide_file, file)
    }
  )
  
  # Handle project creation
  observeEvent(input$createProject, {
    showModal(modalDialog(
      title = "Create New Project",
      size = "l",
      
      fluidRow(
        column(6,
               textInput("creatorName", "Creator Name:*", 
                         placeholder = "Your full name"),
               textInput("creatorEmail", "Email Address:*", 
                         placeholder = "your.email@example.com"),
               textInput("institution", "Institution/School:*", 
                         placeholder = "e.g., Dartmouth College")
        ),
        column(6,
               selectizeInput("expectedParticipants", "Expected Number of Participants:*",
                              choices = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200),
                              selected = 30,
                              options = list(create = TRUE, 
                                             placeholder = 'Select or type a number')),
               dateInput("experimentStartDate", "Experiment Start Date:*", 
                         value = Sys.Date()),
               dateInput("experimentEndDate", "Expected End Date:*", 
                         value = Sys.Date() + 7)
        )
      ),
      
      fluidRow(
        column(12,
               textInput("className", "Class/Group Name:*", 
                         placeholder = "e.g., Biology Spring 2025"),
               selectInput("schoolLevel", "School Level:*",
                           choices = list(
                             "Elementary School" = "elementary",
                             "Middle School" = "middle",
                             "High School" = "high",
                             "College" = "college",
                             "Adult (Non-College)" = "adult"
                           )),
               textAreaInput("projectDescription", "Project Description (Optional):",
                             placeholder = "Any additional information about this project...",
                             rows = 3)
        )
      ),
      
      p("* Required fields", style = "color: #666; font-size: 12px;"),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmCreate", "Create Project", class = "btn-dartmouth")
      )
    ))
  })
  
  observeEvent(input$confirmCreate, {
    # Validate required fields
    if(is.null(input$creatorName) || input$creatorName == "" ||
       is.null(input$creatorEmail) || input$creatorEmail == "" ||
       is.null(input$institution) || input$institution == "" ||
       is.null(input$className) || input$className == "" ||
       is.null(input$expectedParticipants) || input$expectedParticipants == "") {
      showNotification("Please fill in all required fields", type = "error")
      return()
    }
    
    creator_key <- generate_project_key()
    
    # Generate submitter keys based on expected participants
    num_participants <- as.numeric(input$expectedParticipants)
    submitter_keys <- character(num_participants)
    for(i in 1:num_participants) {
      submitter_keys[i] <- generate_project_key()
    }
    
    # Create project info
    school_level_labels <- list(
      "elementary" = "Elementary School",
      "middle" = "Middle School",
      "high" = "High School",
      "college" = "College",
      "adult" = "Adult (Non-College)"
    )
    
    project_info <- list(
      project_name = input$className,
      creator_name = input$creatorName,
      creator_email = input$creatorEmail,
      institution = input$institution,
      class_name = input$className,
      expected_participants = input$expectedParticipants,
      start_date = as.character(input$experimentStartDate),
      end_date = as.character(input$experimentEndDate),
      school_level = input$schoolLevel,
      school_level_display = school_level_labels[[input$schoolLevel]],
      description = input$projectDescription,
      created_timestamp = as.character(Sys.time())
    )
    
    values$currentProject <- list(
      name = input$className,
      key = creator_key,
      creator_key = creator_key,
      submitter_keys = submitter_keys,
      created = Sys.time()
    )
    
    values$projectData <- data.frame()
    values$accessType <- "creator"
    values$continueAddingData <- TRUE  # Enter edit mode immediately
    
    # Store keys for download
    values$tempKeys <- list(
      creator = creator_key,
      submitters = submitter_keys,
      project_name = input$className
    )
    
    save_project(input$className, creator_key, submitter_keys, data.frame(), project_info)
    
    removeModal()
    
    # Show success modal with submitter keys
    showModal(modalDialog(
      title = "Project Created Successfully",
      size = "l",
      h4("Your project has been created!"),
      
      div(
        div(style = "font-size: 14px; color: #666; margin-bottom: 5px;", 
            tags$strong("Creator Key (Full Access):")),
        div(class = "key-display", creator_key)
      ),
      
      div(class = "submitter-keys-display",
          h5(paste("Submitter Keys for", num_participants, "Participants:")),
          p("Each participant should use one unique key to submit their data."),
          lapply(1:length(submitter_keys), function(i) {
            div(class = "submitter-key-item",
                span(paste("Participant", i, ":")),
                span(style = "font-weight: bold;", submitter_keys[i])
            )
          })
      ),
      
      p(style = "margin-top: 20px;", 
        tags$strong("Important:"), 
        "Save these keys! The Creator Key provides full access. Each Submitter Key allows one participant to submit their data once."),
      
      downloadButton("downloadKeys", "Download Keys as Text File", class = "btn btn-info"),
      
      footer = actionButton("continueToWorkspace", "Continue to Data Entry", class = "btn-dartmouth"),
      easyClose = FALSE
    ))
  })
  
  # Download keys handler
  output$downloadKeys <- downloadHandler(
    filename = function() {
      req(values$tempKeys)
      paste0(values$tempKeys$project_name, "_keys_", Sys.Date(), ".txt")
    },
    content = function(file) {
      req(values$tempKeys)
      keys_text <- paste0(
        "Physiological Response Lab - Access Keys\n",
        "===============================================\n",
        "Project: ", values$tempKeys$project_name, "\n",
        "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
        "===============================================\n\n",
        "CREATOR KEY (Full Access - DO NOT SHARE):\n",
        values$tempKeys$creator, "\n\n",
        "-----------------------------------------------\n",
        "SUBMITTER KEYS (Distribute one key per participant):\n",
        "-----------------------------------------------\n",
        paste0("Participant ", 1:length(values$tempKeys$submitters), ": ", 
               values$tempKeys$submitters, collapse = "\n")
      )
      writeLines(keys_text, file)
    }
  )
  
  observeEvent(input$continueToWorkspace, {
    values$currentState <- "workspace"
    removeModal()
  })
  
  # Handle project recall by creator key or submitter key
  observeEvent(input$openRecall, {
    showModal(modalDialog(
      title = "Recall Project",
      textInput("recallKey", "Enter Project Key:", 
                placeholder = "8-character key (Creator or Submitter)"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmRecall", "Open", class = "btn-dartmouth")
      )
    ))
  })
  
  observeEvent(input$confirmRecall, {
    req(input$recallKey)
    
    project_data <- load_project(input$recallKey)
    
    if (!is.null(project_data)) {
      values$currentProject <- list(
        name = project_data$metadata$project_name,
        key = project_data$metadata$creator_key,
        creator_key = project_data$metadata$creator_key,
        submitter_keys = project_data$metadata$submitter_keys,
        created = project_data$metadata$created_date
      )
      values$projectData <- if(is.null(project_data$data)) data.frame() else project_data$data
      values$accessType <- project_data$access_type
      
      if (project_data$access_type == "submitter") {
        values$submitterKey <- project_data$submitter_key
        
        # Check if submitter has already submitted
        if (project_data$has_submitted) {
          showModal(modalDialog(
            title = "Data Already Submitted",
            p("You have already submitted data with this key."),
            p("You can view project statistics but cannot submit new data."),
            footer = tagList(
              modalButton("Cancel"),
              actionButton("viewOnlyMode", "Continue to View", class = "btn btn-info")
            )
          ))
          return()
        }
      }
      
      values$currentState <- "workspace"
      removeModal()
      
      # For creators, ask if they want to continue adding data
      if (project_data$access_type == "creator" && nrow(values$projectData) > 0) {
        showModal(modalDialog(
          title = "Project Loaded",
          p(paste("Project:", project_data$metadata$project_name)),
          p(paste("Current participants:", length(unique(values$projectData$id)))),
          h5("Would you like to:"),
          footer = tagList(
            actionButton("continueAddingExisting", "Continue Adding Data", class = "btn btn-primary"),
            actionButton("viewDataOnly", "View Data Only", class = "btn btn-info")
          )
        ))
      } else if (project_data$access_type == "creator") {
        values$continueAddingData <- TRUE
      }
      
      showNotification("Project loaded successfully!", type = "message")
    } else {
      showNotification("Invalid project key or project not found", type = "error")
    }
  })
  
  # Handle view only mode for submitters who already submitted
  observeEvent(input$viewOnlyMode, {
    values$currentState <- "workspace"
    removeModal()
  })
  
  # Handle continue adding for existing projects
  observeEvent(input$continueAddingExisting, {
    values$continueAddingData <- TRUE
    removeModal()
  })
  
  observeEvent(input$viewDataOnly, {
    values$continueAddingData <- FALSE
    removeModal()
  })
  
  # Runs a change to the project file. If it fails, the user is told and
  # NULL is returned.
  try_save <- function(expr) {
    tryCatch(expr, error = function(e) {
      showNotification(paste("Could not save:", conditionMessage(e)), type = "error")
      NULL
    })
  }
  
  # Reload the project data from disk. Every change is written when it is
  # made, so there is nothing to save; this shows what others have submitted.
  observeEvent(input$refreshProject, {
    req(values$currentProject)
    values$projectData <- read_project_data(values$currentProject$creator_key)
    showNotification(paste("Data reloaded. Participants:", length(unique(values$projectData$id))),
                     type = "message")
  })
  
  # Handle close project
  observeEvent(input$closeProject, {
    values$currentState <- "home"
    values$currentProject <- NULL
    values$projectData <- data.frame()
    values$uploadMethod <- NULL
    values$accessType <- NULL
    values$submitterKey <- NULL
    values$continueAddingData <- FALSE
  })
  
  # Upload interface
  output$uploadInterface <- renderUI({
    # Nothing to show when no project is open (it was just closed)
    req(values$accessType)
    
    # Check if submitter has already submitted
    if (values$accessType == "submitter") {
      # %in% and not ==: records from the creator have no submitter key, and
      # a comparison with NA would select them
      submitter_data <- values$projectData[values$projectData$submitter_key %in% values$submitterKey, ]
      if (nrow(submitter_data) > 0) {
        return(
          div(class = "upload-success",
              h4("Data Already Submitted"),
              p("You have already submitted data with this key."),
              p(paste("Your ID:", submitter_data$id[1])),
              p("Contact the project creator if you need to modify your submission.")
          )
        )
      }
    }
    
    # Show existing data for creators
    if (values$accessType == "creator" && nrow(values$projectData) > 0 && !values$continueAddingData) {
      return(
        tagList(
          div(class = "upload-success",
              h4("Data already exists in this project"),
              p(sprintf("Current participants: %d", length(unique(values$projectData$id)))),
              actionButton("startAddingData", "Continue Adding Data", 
                           class = "btn-dartmouth", icon = icon("plus"))
          ),
          hr(),
          h4("Uploaded Data Management"),
          p(paste("Total participants:", length(unique(values$projectData$id)))),
          DT::dataTableOutput("uploadedDataTable")
        )
      )
    }
    
    # Data entry mode
    if (values$continueAddingData || values$accessType == "submitter") {
      if (is.null(values$uploadMethod)) {
        # Choose upload method
        tagList(
          h3("Select Data Upload Method", style = "text-align: center; margin-bottom: 30px;"),
          fluidRow(
            column(6,
                   div(class = "selection-card",
                       icon("keyboard", class = "fa-3x", style = "color: #4a9b6f; margin-bottom: 20px;"),
                       h4("Manual Data Entry"),
                       p("Enter data directly into the application form"),
                       actionButton("selectManual", "Select Manual Entry", 
                                    class = "btn-dartmouth btn-block")
                   )
            ),
            column(6,
                   div(class = "selection-card",
                       icon("file-upload", class = "fa-3x", style = "color: #4a9b6f; margin-bottom: 20px;"),
                       h4("File Upload"),
                       p("Upload CSV or Excel file with multiple participants"),
                       if (values$accessType == "submitter") {
                         p(style = "color: red;", "Note: File upload is only available for creators")
                       } else {
                         actionButton("selectFile", "Select File Upload", 
                                      class = "btn-dartmouth btn-block")
                       }
                   )
            )
          ),
          if (values$accessType == "creator") {
            tagList(
              hr(),
              actionButton("stopAddingData", "Cancel", class = "btn btn-default")
            )
          }
        )
      } else if (values$uploadMethod == "manual") {
        # Manual entry form
        tagList(
          actionButton("backToUploadChoice", "← Change Upload Method", class = "btn btn-default"),
          hr(),
          h4("Manual Data Entry"),
          
          textInput("participantID", "Participant ID:", 
                    placeholder = "e.g., student01",
                    value = if(values$accessType == "submitter") values$submitterKey else ""),
          
          if (values$accessType == "submitter") {
            p(style = "color: #666; font-style: italic;", 
              "Your ID has been auto-filled with your submitter key")
          },
          
          fluidRow(
            lapply(1:length(experiment_sections), function(i) {
              section <- experiment_sections[i]
              prefix <- section_prefixes[i]
              
              column(6,
                     div(class = "phase-box",
                         h5(strong(paste0(i, ". ", section))),
                         
                         numericInput(paste0(prefix, "_hr30_input"), 
                                      "30-second pulse count:", 
                                      value = NA, min = 10, max = 60),
                         
                         uiOutput(paste0(prefix, "_bpm_display")),
                         
                         if(section == "Cognitive Test") {
                           tagList(
                             hr(),
                             numericInput(paste0(prefix, "_gamescore_input"),
                                          "Cognitive Test Score:",
                                          value = NA, min = 0)
                           )
                         },
                         
                         hr(),
                         
                         h6("Self-Report Measures (1-10 scale):"),
                         
                         lapply(names(affect_measures), function(measure) {
                           sliderInput(paste0(prefix, "_", measure, "_input"),
                                       affect_measures[measure],
                                       min = 1, max = 10, value = 5,
                                       step = 1, ticks = TRUE)
                         })
                     )
              )
            })
          ),
          
          br(),
          actionButton("saveManualData", "Save Data", 
                       icon = icon("save"),
                       class = "btn-dartmouth btn-lg btn-block")
        )
      } else if (values$uploadMethod == "file") {
        # File upload form
        tagList(
          actionButton("backToUploadChoice", "← Change Upload Method", class = "btn btn-default"),
          hr(),
          h4("File Upload"),
          
          fileInput("uploadFile", "Choose File:",
                    accept = c(".csv", ".xlsx", ".xls")),
          
          downloadButton("downloadTemplate", "Download Template", 
                         class = "btn-dartmouth"),
          
          uiOutput("uploadStatus")
        )
      }
    }
  })
  
  # Handle start adding data
  observeEvent(input$startAddingData, {
    values$continueAddingData <- TRUE
    values$uploadMethod <- NULL
  })
  
  # Handle stop adding data
  observeEvent(input$stopAddingData, {
    values$continueAddingData <- FALSE
    values$uploadMethod <- NULL
  })
  
  # Upload method selection
  observeEvent(input$selectManual, {
    values$uploadMethod <- "manual"
  })
  
  observeEvent(input$selectFile, {
    if (values$accessType != "submitter") {
      values$uploadMethod <- "file"
    }
  })
  
  observeEvent(input$backToUploadChoice, {
    values$uploadMethod <- NULL
  })
  
  # Calculate and display BPM
  lapply(section_prefixes, function(prefix) {
    output[[paste0(prefix, "_bpm_display")]] <- renderUI({
      pulse_count <- input[[paste0(prefix, "_hr30_input")]]
      if(!is.null(pulse_count) && !is.na(pulse_count) && pulse_count > 0) {
        bpm <- pulse_count * 2
        tags$p(
          style = "color: #00693e; font-weight: bold;",
          sprintf("Calculated heart rate: %d BPM", bpm)
        )
      }
    })
  })
  
  # Save manual data
  observeEvent(input$saveManualData, {
    if(is.null(input$participantID) || trimws(input$participantID) == "") {
      showNotification("Please enter participant ID", type = "error")
      return()
    }
    
    new_row <- data.frame(
      row_id = NA_real_,  # given when the record is added to the project file
      id = trimws(input$participantID),
      timestamp = Sys.time(),
      submitter_key = if(values$accessType == "submitter") values$submitterKey else NA,
      stringsAsFactors = FALSE
    )
    
    for(i in 1:length(section_prefixes)) {
      prefix <- section_prefixes[i]
      
      hr_value <- input[[paste0(prefix, "_hr30_input")]]
      new_row[[paste0(prefix, "_hr30")]] <- ifelse(is.na(hr_value), NA, hr_value)
      
      if(prefix == "cognitive") {
        score_value <- input$cognitive_gamescore_input
        new_row$cognitive_gamescore <- ifelse(is.na(score_value), NA, score_value)
      }
      
      for(measure in names(affect_measures)) {
        value <- input[[paste0(prefix, "_", measure, "_input")]]
        new_row[[paste0(prefix, "_", measure)]] <- value
      }
    }
    
    # Add the record to the project file. The duplicate check is made there,
    # against the records on disk.
    result <- try_save(add_records(values$currentProject$creator_key, new_row))
    if (is.null(result)) {
      return()
    }
    values$projectData <- result$data
    if (!result$added) {
      showNotification("This ID already exists in the project!", type = "error")
      return()
    }
    
    if (values$accessType == "submitter") {
      showNotification("Data submitted successfully!", type = "message")
      values$continueAddingData <- FALSE
      values$uploadMethod <- NULL
    } else {
      # For creators, just show success notification and clear form
      showNotification(paste("Data saved for participant", input$participantID), type = "message")
      
      # Clear inputs for next entry
      updateTextInput(session, "participantID", value = "")
      for(prefix in section_prefixes) {
        updateNumericInput(session, paste0(prefix, "_hr30_input"), value = NA)
        if(prefix == "cognitive") {
          updateNumericInput(session, "cognitive_gamescore_input", value = NA)
        }
        for(measure in names(affect_measures)) {
          updateSliderInput(session, paste0(prefix, "_", measure, "_input"), value = 5)
        }
      }
    }
  })
  
  # Uploaded data table
  output$uploadedDataTable <- DT::renderDataTable({
    if(nrow(values$projectData) == 0) {
      return(data.frame(Message = "No data uploaded yet"))
    }
    
    uploaded_data_table(values$projectData, values$accessType == "creator")
  })
  
  # Handle row deletion
  observeEvent(input$deleteRow, {
    if (values$accessType == "creator") {
      data <- try_save(delete_record(values$currentProject$creator_key, input$deleteRow))
      if (is.null(data)) {
        return()
      }
      values$projectData <- data
      showNotification("Row deleted", type = "message")
    }
  })
  
  # Handle file upload
  observeEvent(input$uploadFile, {
    req(input$uploadFile)
    
    if (values$accessType == "submitter") {
      showNotification("File upload is only available for creators", type = "error")
      return()
    }
    
    tryCatch({
      ext <- tools::file_ext(input$uploadFile$datapath)
      
      if(ext == "csv") {
        df <- read.csv(input$uploadFile$datapath, stringsAsFactors = FALSE, 
                       na.strings = c("", "NA", "N/A", " "))
      } else if(ext %in% c("xlsx", "xls")) {
        df <- read_excel(input$uploadFile$datapath, na = c("", "NA", "N/A", " "))
      } else {
        stop("Unsupported file format")
      }
      
      # Clean column names (netid becomes id)
      names(df) <- clean_column_names(names(df))
      
      # Detect and remove header rows
      header_rows <- detect_header_rows(df)
      if (length(header_rows) > 0) {
        df <- df[-header_rows, ]
      }
      
      # Remove empty rows
      empty_rows <- which(apply(df, 1, function(x) all(is.na(x) | x == "")))
      if (length(empty_rows) > 0) {
        df <- df[-empty_rows, ]
      }
      
      # Validate data
      validation_report <- validate_uploaded_data(df)
      
      if(validation_report$has_errors) {
        showNotification("Data validation failed. Please check your file format.", type = "error")
        return()
      }
      
      # Keep the ID and the protocol columns, as numbers
      df <- keep_protocol_columns(df)
      
      # Remove invalid rows
      df <- df[!is.na(df$id) & df$id != "" & !grepl("\\{|\\}|Import|QID", df$id), ]
      
      # Add metadata
      df$timestamp <- Sys.time()
      df$submitter_key <- NA
      
      # Add the records to the project file. The duplicate check is made
      # there, against the records on disk.
      result <- add_records(values$currentProject$creator_key, df)
      values$projectData <- result$data
      if (!result$added) {
        showNotification(paste("Duplicate IDs found:", paste(result$duplicates, collapse = ", ")), 
                         type = "error")
        return()
      }
      
      showNotification(paste("Successfully uploaded", nrow(df), "records."), type = "message")
      
      # Reset file input
      shinyjs::reset("uploadFile")
      
    }, error = function(e) {
      showNotification(paste("Upload failed:", e$message), type = "error")
    })
  })
  
  # Handle upload more
  observeEvent(input$uploadMore, {
    removeModal()
    values$uploadMethod <- NULL
  })
  
  # Handle finish uploading
  observeEvent(input$finishUploading, {
    removeModal()
    values$continueAddingData <- FALSE
    values$uploadMethod <- NULL
  })
  
  # Download template
  output$downloadTemplate <- downloadHandler(
    filename = function() {
      paste("data_template_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      template <- data.frame(
        id = c("example1", "example2", "example3")
      )
      
      for(prefix in section_prefixes) {
        template[[paste0(prefix, "_hr30")]] <- c(35, 38, 33)
        
        for(measure in names(affect_measures)) {
          template[[paste0(prefix, "_", measure)]] <- c(5, 6, 4)
        }
      }
      
      template$cognitive_gamescore <- c(85, 92, 78)
      
      write.csv(template, file, row.names = FALSE)
    }
  )
  
  # Limited data preview for submitters
  output$limitedDataPreview <- renderUI({
    if(nrow(values$projectData) == 0) {
      return(p("No data in project yet."))
    }
    
    tagList(
      h4("Project Statistics"),
      p(paste("Total participants:", length(unique(values$projectData$id)))),
      p(paste("Total records:", nrow(values$projectData))),
      p(paste("Last update:", format(max(values$projectData$timestamp), "%Y-%m-%d %H:%M")))
    )
  })
  
  # Full data preview for creators
  output$dataPreview <- renderUI({
    if(nrow(values$projectData) == 0) {
      return(p("No data to preview yet. Please upload data first."))
    }
    
    tagList(
      h4("Data Completion Overview"),
      uiOutput("completionOverview"),
      
      h4("Heart Rate Data (BPM)"),
      DT::dataTableOutput("heartRatePreview") %>% withSpinner(),
      br(),
      h4("Self-Report Affect Measures"),
      DT::dataTableOutput("affectPreview") %>% withSpinner(),
      br(),
      h4("Statistical Summary"),
      verbatimTextOutput("statSummary")
    )
  })
  
  # Completion overview
  output$completionOverview <- renderUI({
    if(nrow(values$projectData) == 0) return(NULL)
    
    data <- values$projectData
    total_participants <- nrow(data)
    
    # Calculate completion for heart rate
    hr_complete <- sum(complete.cases(data[paste0(section_prefixes, "_hr30")]))
    hr_percentage <- round((hr_complete / total_participants) * 100, 1)
    
    # Calculate completion for affect measures
    affect_cols <- c()
    for(prefix in section_prefixes) {
      for(measure in names(affect_measures)) {
        affect_cols <- c(affect_cols, paste0(prefix, "_", measure))
      }
    }
    affect_complete <- sum(complete.cases(data[affect_cols]))
    affect_percentage <- round((affect_complete / total_participants) * 100, 1)
    
    div(class = "completion-grid",
        div(class = "completion-card",
            div(class = "completion-percentage", paste0(hr_percentage, "%")),
            div(class = "completion-label", "Heart Rate Complete")
        ),
        div(class = "completion-card",
            div(class = "completion-percentage", paste0(affect_percentage, "%")),
            div(class = "completion-label", "Affect Measures Complete")
        ),
        div(class = "completion-card",
            div(class = "completion-percentage", total_participants),
            div(class = "completion-label", "Total Participants")
        )
    )
  })
  
  # Heart rate preview
  output$heartRatePreview <- DT::renderDataTable({
    if(nrow(values$projectData) == 0) return(NULL)
    
    hr_data <- values$projectData %>%
      select(id, all_of(paste0(section_prefixes, "_hr30")))
    
    hr_cols <- paste0(section_prefixes, "_hr30")
    hr_data[hr_cols] <- hr_data[hr_cols] * 2  # Convert to BPM
    
    names(hr_data) <- c("Participant", paste0(experiment_sections, " (BPM)"))
    
    datatable(hr_data, options = list(scrollX = TRUE))
  })
  
  # Affect preview
  output$affectPreview <- DT::renderDataTable({
    if(nrow(values$projectData) == 0) return(NULL)
    
    affect_data <- values$projectData %>%
      select(id, matches("_(stress|neck|chest|sweat|breath|mouth)$")) %>%
      pivot_longer(cols = -id,
                   names_to = c("section", "measure"),
                   names_pattern = "(.*)_(.*)",
                   values_to = "value") %>%
      mutate(
        section = factor(section, levels = section_prefixes,
                         labels = experiment_sections),
        measure = factor(measure, levels = names(affect_measures),
                         labels = affect_measures)
      ) %>%
      pivot_wider(names_from = section, values_from = value)
    
    datatable(affect_data, options = list(scrollX = TRUE))
  })
  
  # Statistical summary
  output$statSummary <- renderPrint({
    if(nrow(values$projectData) == 0) {
      cat("No data available for analysis")
      return()
    }
    
    data <- values$projectData
    
    cat("=== Statistical Summary ===\n\n")
    cat("Project:", values$currentProject$name, "\n")
    cat("Total Participants:", length(unique(data$id)), "\n\n")
    
    cat("Heart Rate Summary (BPM):\n")
    cat("Section                      n    Mean    SD      Min     Max\n")
    cat("------------------------------------------------------------\n")
    
    for(i in 1:length(experiment_sections)) {
      hr_data <- data[[paste0(section_prefixes[i], "_hr30")]] * 2
      hr_data <- hr_data[!is.na(hr_data)]
      
      if(length(hr_data) > 0) {
        cat(sprintf("%-25s %4d  %6.1f  %6.1f  %6.0f  %6.0f\n",
                    experiment_sections[i],
                    length(hr_data),
                    mean(hr_data),
                    sd(hr_data),
                    min(hr_data),
                    max(hr_data)))
      } else {
        cat(sprintf("%-25s    0     N/A     N/A     N/A     N/A\n", experiment_sections[i]))
      }
    }
  })
  
  # Data Analysis: only for creators
  output$dataAnalysis <- renderUI({
    # Nothing to show when no project is open (it was just closed)
    req(values$accessType)
    
    if(values$accessType != "creator") {
      return(p("Analysis features are only available for project creators."))
    }
    
    if(nrow(values$projectData) == 0) {
      return(p("No data to analyze yet. Please upload data first."))
    }
    
    tagList(
      tabsetPanel(
        tabPanel("Heart Rate Analysis",
                 plotlyOutput("heartRatePlot", height = "500px") %>% withSpinner(),
                 br(),
                 h4("Statistical Tests"),
                 tableOutput("heartRateStats")
        ),
        
        tabPanel("Affect Measures Analysis",
                 selectInput("affectMeasure", "Select Measure:",
                             choices = names(affect_measures),
                             selected = "stress"),
                 plotlyOutput("affectPlot", height = "500px") %>% withSpinner()
        )
      )
    )
  })
  
  # Heart rate plot
  output$heartRatePlot <- renderPlotly({
    if(nrow(values$projectData) == 0) return(NULL)
    
    hr_data <- values$projectData %>%
      select(id, all_of(paste0(section_prefixes, "_hr30"))) %>%
      mutate(across(all_of(paste0(section_prefixes, "_hr30")), ~. * 2)) %>%
      pivot_longer(cols = -id,
                   names_to = "section",
                   values_to = "bpm") %>%
      mutate(section = factor(section,
                              levels = paste0(section_prefixes, "_hr30"),
                              labels = experiment_sections))
    
    plot_ly(hr_data, x = ~section, y = ~bpm, type = "box",
            marker = list(color = "#00693e")) %>%
      layout(title = "Heart Rate Distribution Across Sections",
             xaxis = list(title = ""),
             yaxis = list(title = "Heart Rate (BPM)"),
             showlegend = FALSE)
  })
  
  # Heart rate statistics
  output$heartRateStats <- renderTable({
    if(nrow(values$projectData) < 3) {
      return(data.frame(Message = "Need at least 3 participants for statistical tests"))
    }
    
    data <- values$projectData
    baseline_hr <- data$baseline_hr30 * 2
    
    results <- data.frame(
      Section = experiment_sections[experiment_sections != "Baseline"],
      `Mean Difference (BPM)` = numeric(6),
      `P-value` = character(6),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    idx <- 1
    for(i in 1:length(section_prefixes)) {
      if(experiment_sections[i] != "Baseline") {
        section_hr <- data[[paste0(section_prefixes[i], "_hr30")]] * 2
        
        if(sum(!is.na(baseline_hr) & !is.na(section_hr)) >= 3) {
          diff <- mean(section_hr - baseline_hr, na.rm = TRUE)
          test <- wilcox.test(baseline_hr, section_hr, paired = TRUE, exact = FALSE)
          
          results$`Mean Difference (BPM)`[idx] <- round(diff, 1)
          results$`P-value`[idx] <- ifelse(test$p.value < 0.001, "< 0.001", 
                                           sprintf("%.3f", test$p.value))
        } else {
          results$`Mean Difference (BPM)`[idx] <- NA
          results$`P-value`[idx] <- "N/A"
        }
        idx <- idx + 1
      }
    }
    
    results
  }, striped = TRUE, hover = TRUE)
  
  # Affect plot
  output$affectPlot <- renderPlotly({
    req(input$affectMeasure)
    if(nrow(values$projectData) == 0) return(NULL)
    
    affect_data <- values$projectData %>%
      select(id, all_of(paste0(section_prefixes, "_", input$affectMeasure))) %>%
      pivot_longer(cols = -id,
                   names_to = "section",
                   values_to = "rating") %>%
      mutate(section = factor(section,
                              levels = paste0(section_prefixes, "_", input$affectMeasure),
                              labels = experiment_sections))
    
    plot_ly(affect_data, x = ~section, y = ~rating, type = "box",
            marker = list(color = "#4a9b6f")) %>%
      layout(title = paste("Distribution of", affect_measures[input$affectMeasure]),
             xaxis = list(title = ""),
             yaxis = list(title = "Rating (1-10)", range = c(0, 11)),
             showlegend = FALSE)
  })
  
  # Report Generation: only for creators
  output$reportGeneration <- renderUI({
    # Nothing to show when no project is open (it was just closed)
    req(values$accessType)
    
    if(values$accessType != "creator") {
      return(p("Report generation is only available for project creators."))
    }
    
    if(nrow(values$projectData) == 0) {
      return(p("No data available for report generation."))
    }
    
    tagList(
      fluidRow(
        column(6,
               h4("Generate Individual Reports"),
               selectInput("reportParticipant", "Select Participant:",
                           choices = unique(values$projectData$id)),
               br(),
               actionButton("generateReport", "Generate Report",
                            class = "btn-dartmouth"),
               br(), br(),
               conditionalPanel(
                 condition = "input.generateReport > 0",
                 downloadButton("downloadReport", "Download Report",
                                class = "btn btn-primary")
               )
        ),
        column(6,
               h4("Export Complete Dataset"),
               p("Download the complete dataset with all variables"),
               downloadButton("downloadAllData", "Download Complete Data (CSV)",
                              class = "btn btn-success btn-lg")
        )
      )
    )
  })
  
  # Store report data
  report_data <- reactiveVal(NULL)
  
  # Generate report
  observeEvent(input$generateReport, {
    req(input$reportParticipant)
    
    participant_data <- values$projectData %>%
      filter(id == input$reportParticipant) %>%
      slice(1)
    
    report_data(participant_data)
    showNotification("Report generated successfully!", type = "message")
  })
  
  # Download report
  output$downloadReport <- downloadHandler(
    filename = function() {
      safe_id <- gsub("[^A-Za-z0-9_-]", "_", input$reportParticipant)
      paste0(safe_id, "_PhysiologyReport_", Sys.Date(), ".html")
    },
    content = function(file) {
      participant_data <- report_data()
      
      # Generate HTML report
      html_content <- generate_html_report(participant_data, values$projectData, input$reportParticipant)
      writeLines(html_content, file)
    }
  )
  
  # Download all data
  output$downloadAllData <- downloadHandler(
    filename = function() {
      paste0(values$currentProject$name, "_complete_dataset_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Export complete dataset
      complete_data <- values$projectData %>%
        select(-any_of(c("row_id", "submitter_key")))
      
      write.csv(complete_data, file, row.names = FALSE)
    }
  )
}

# Function to generate HTML report
generate_html_report <- function(individual_data, class_data, participant_id) {
  
  # The ID is typed by the participant and goes into HTML below.
  participant_id <- htmltools::htmlEscape(participant_id)
  
  n_participants <- length(unique(class_data$id))
  
  # Calculate heart rate data
  hr_individual <- individual_data %>%
    select(all_of(paste0(section_prefixes, "_hr30"))) %>%
    mutate_all(~. * 2) %>%
    as.numeric()
  
  hr_class_mean <- class_data %>%
    select(all_of(paste0(section_prefixes, "_hr30"))) %>%
    mutate_all(~. * 2) %>%
    summarise_all(~mean(., na.rm = TRUE)) %>%
    as.numeric()
  
  hr_class_sd <- class_data %>%
    select(all_of(paste0(section_prefixes, "_hr30"))) %>%
    mutate_all(~. * 2) %>%
    summarise_all(~sd(., na.rm = TRUE)) %>%
    as.numeric()
  
  # HTML template
  html_content <- paste0('
<!DOCTYPE html>
<html>
<head>
  <title>Physiological Response Analysis Report - ', participant_id, '</title>
  <style>
    body { 
      font-family: "Open Sans", Arial, sans-serif; 
      margin: 40px;
      color: #333;
      line-height: 1.6;
    }
    .header { 
      display: flex; 
      justify-content: space-between; 
      border-bottom: 3px solid #00693e; 
      padding-bottom: 20px; 
      margin-bottom: 30px;
    }
    h1, h2, h3 { color: #00693e; }
    table { 
      border-collapse: collapse; 
      width: 100%; 
      margin-bottom: 20px; 
    }
    th, td { 
      border: 1px solid #ddd; 
      padding: 10px; 
    }
    th { 
      background-color: #00693e; 
      color: white; 
    }
    tr:nth-child(even) {
      background-color: #f9f9f9;
    }
    .summary-box { 
      background-color: #e8f4ed; 
      padding: 20px; 
      border-radius: 8px; 
      margin-bottom: 25px;
      border: 1px solid #00693e;
    }
    .note {
      background-color: #f5f5f5;
      padding: 15px;
      border-left: 4px solid #00693e;
      margin-top: 20px;
    }
  </style>
</head>
<body>
  <div class="header">
    <div>
      <h1>Physiological Response Analysis Report</h1>
      <p><strong>Participant ID:</strong> ', participant_id, '</p>
      <p><strong>Report Generated:</strong> ', format(Sys.Date(), "%B %d, %Y"), '</p>
    </div>
  </div>
  
  <div class="summary-box">
    <h3>Study Overview</h3>
    <p>This report summarizes your physiological and self-reported responses during the in-class activity.</p>
    <p><strong>Total participants in class:</strong> ', n_participants, '</p>
  </div>
  
  <h2>1. Heart Rate Analysis</h2>
  <table>
    <tr>
      <th>Section</th>
      <th>Your Heart Rate (BPM)</th>
      <th>Class Average ± SD</th>
      <th>Difference from Class</th>
    </tr>')
  
  for(i in 1:length(experiment_sections)) {
    diff <- round(hr_individual[i] - hr_class_mean[i], 1)
    diff_text <- ifelse(diff > 0, paste0("+", diff), as.character(diff))
    
    html_content <- paste0(html_content, '
    <tr>
      <td>', experiment_sections[i], '</td>
      <td>', ifelse(is.na(hr_individual[i]), "N/A", hr_individual[i]), '</td>
      <td>', round(hr_class_mean[i], 1), ' ± ', round(hr_class_sd[i], 1), '</td>
      <td>', ifelse(is.na(hr_individual[i]), "N/A", diff_text), '</td>
    </tr>')
  }
  
  html_content <- paste0(html_content, '
  </table>
  
  <h2>2. Self-Report Affect Measures</h2>')
  
  for(measure in names(affect_measures)) {
    html_content <- paste0(html_content, '
  <h3>', affect_measures[measure], '</h3>
  <table>
    <tr>
      <th>Section</th>
      <th>Your Rating</th>
      <th>Class Average</th>
    </tr>')
    
    for(i in 1:length(experiment_sections)) {
      ind_rating <- individual_data[[paste0(section_prefixes[i], "_", measure)]]
      class_mean <- mean(class_data[[paste0(section_prefixes[i], "_", measure)]], na.rm = TRUE)
      
      html_content <- paste0(html_content, '
    <tr>
      <td>', experiment_sections[i], '</td>
      <td>', ind_rating, '</td>
      <td>', round(class_mean, 1), '</td>
    </tr>')
    }
    
    html_content <- paste0(html_content, '</table>')
  }
  
  if("cognitive_gamescore" %in% names(individual_data)) {
    cognitive_score <- individual_data$cognitive_gamescore
    if(!is.na(cognitive_score)) {
      class_cognitive_mean <- mean(class_data$cognitive_gamescore, na.rm = TRUE)
      
      html_content <- paste0(html_content, '
  <h2>3. Cognitive Test Performance</h2>
  <table>
    <tr>
      <th>Measure</th>
      <th>Your Score</th>
      <th>Class Average</th>
    </tr>
    <tr>
      <td>Cognitive Test Score</td>
      <td>', cognitive_score, '</td>
      <td>', round(class_cognitive_mean, 1), '</td>
    </tr>
  </table>')
    }
  }
  
  html_content <- paste0(html_content, '
  <div class="note">
    <h4>Important Notes:</h4>
    <ul>
      <li>Heart rate measurements are based on 30-second pulse counts, converted to beats per minute (BPM)</li>
      <li>Individual variations in physiological responses are normal and expected</li>
      <li>This report is for educational purposes to help understand physiological stress responses</li>
    </ul>
  </div>
</body>
</html>')
  
  return(html_content)
}

# Run the application
shinyApp(ui = ui, server = server)