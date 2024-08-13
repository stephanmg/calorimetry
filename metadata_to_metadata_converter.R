library(shiny)
library(shinyjs)  # For collapsible elements
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(hms)
library(lubridate)

date_columns <- c("dob", "Date End",  "Date Start", "date body start", "date body end")
time_columns <- c("Time End", "Time Start")

transform_df <- function(df) {

   # Step 2: Define the vector of columns you want to select
   selected_columns <- c("Animal #", "sex", "genotype", "delta_bm", "age at start", "diet",  "bw start", "bw end", "lm start", "lm end", "fm start", "fm end")  # Specify your desired columns
   metadata_column_names <- c("personal_ID", "sex", "genotype_group", "delta_bm", "age", "diet_group", "body weight", "bw_end", "lean_mass", "lm_end", "fat_mass", "fm_end") # Rename columns

   rename_map <- setNames(metadata_column_names, selected_columns)
   # Step 3: Select only the specified columns
   df_selected <- df %>% select(all_of(selected_columns))

   # Step 4: Transpose the row data so that each row becomes a column
   df_transposed <- t(df_selected) %>% as.data.frame()

   # Step 5: Add the original column names as the first column
   df_transposed <- df_transposed %>%
     mutate(Original_Column = rownames(df_transposed)) %>%
     relocate(Original_Column, .before = everything())

   df_transposed <- df_transposed %>% mutate(Original_Column = recode(Original_Column, !!!rename_map))
   df_transposed <- rbind(rep("", length(colnames(df_transposed))), df_transposed)
   df_transposed <- rbind(c("Sample-Section", rep("", length(colnames(df_transposed))-1)), df_transposed)
   df_transposed <- rbind(df_transposed, c("Sub-Sample Section", rep("", length(colnames(df_transposed))-1)))
   return(df_transposed)
}

# Define UI for the Shiny app
ui <- fluidPage(
  
  useShinyjs(),  # Initialize shinyjs
  
  # Add custom CSS for pastel color theme and input field highlights
  tags$style(HTML("
    body {
      background-color: #f7f7f7;
      color: #333333;
    }
    .sidebarPanel {
      background-color: #ffffff;
      border: 1px solid #e0e0e0;
      padding: 15px;
      border-radius: 10px;
    }
    .btn-primary {
      background-color: #c2e1f5;
      border-color: #c2e1f5;
      color: #333333;
    }
    .cohort-btn-1 {
      background-color: #FFD1DC;  /* Pastel Pink */
      border-color: #FFD1DC;
      color: #333333;
    }
    .cohort-btn-2 {
      background-color: #C1E1C1;  /* Pastel Green */
      border-color: #C1E1C1;
      color: #333333;
    }
    .cohort-btn-3 {
      background-color: #F0E68C;  /* Pastel Yellow */
      border-color: #F0E68C;
      color: #333333;
    }
    .cohort-btn-4 {
      background-color: #ADD8E6;  /* Pastel Blue */
      border-color: #ADD8E6;
      color: #333333;
    }
    .text-input-1 input {
      background-color: #FFE4E9 !important;  /* Lighter shade of Pastel Pink */
    }
    .text-input-2 input {
      background-color: #E3F5E3 !important;  /* Lighter shade of Pastel Green */
    }
    .text-input-3 input {
      background-color: #FAFAD2 !important;  /* Lighter shade of Pastel Yellow */
    }
    .text-input-4 input {
      background-color: #E0FFFF !important;  /* Lighter shade of Pastel Blue */
    }
    .form-control {
      border-radius: 5px;
      border-color: #d0d0d0;
    }
    .shiny-input-container {
      margin-bottom: 20px;
    }
    hr {
      border-color: #e0e0e0;
    }
    .shiny-download-link, .shiny-download-link:visited {
      color: #0073b7;
    }
    .well {
      background-color: #fafafa;
      border: 1px solid #e0e0e0;
      border-radius: 10px;
    }
  ")),
  
  # App title
  titlePanel("Metadata to Metadata sheet converter"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      conditionalPanel("input.specify_manually == false", fileInput("file1", "Choose metadata from another Excel file", accept = c(".xlsx"))),
      conditionalPanel("input.specify_manually == false", 
           actionButton("display_excel", "Display input Excel file"),
           actionButton("display_metadata", "Display metadata sheet")
      ),
      conditionalPanel("input.specify_manually == true", 
           actionButton("display_metadata", "Display metadata sheet")
      ),
      checkboxInput("specify_manually", "Specify metadata instead manually?", value = FALSE),
      conditionalPanel("input.specify_manually == true", 
         numericInput("n_cohorts", "How many cohorts?", value = 1, min = 1, max = 4),
         actionButton("generate_cohorts", "Generate Cohort Inputs", class = "btn-primary"),
         uiOutput("cohort_name_inputs"),  # Dynamic input for Versuchsbezeichnung and DataFile
      ),
      selectInput("select_columns", "Select columns", 
                     choices = c("Cohort", "Box", "Animal #", "dob", "sex", "genotype", "diet", "age at start", 
                                 "date body start", "date body end", "bw start", "bw end", "delta_bm", 
                                 "lm start", "lm end", "fm start", "fm end", "ff start", "ff end", 
                                 "Date Start", "Time Start", "Date End", "Time End"), 
                     selected = c("Cohort", "Box", "Animal #", "dob", "sex", "genotype", "diet", "age at start", 
                                  "date body start", "date body end", "bw start", "bw end", "delta_bm", 
                                  "lm start", "lm end", "fm start", "fm end", "ff start", "ff end", 
                                  "Date Start", "Time Start", "Date End", "Time End"), 
                     multiple = TRUE),
      # actionButton("process", "Process File", class = "btn-primary"),
      downloadButton("downloadData", "Download Processed File", class = "btn-primary"),
      downloadButton("downloadMetadata", "Download metadata sheet", class = "btn-primary")
      #uiOutput("status")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      uiOutput("dynamic_cohort_inputs"),  # Dynamic inputs for cohorts
      uiOutput("uploaded_file_table_header"),
      tableOutput("uploaded_file_table"), # Display uploaded Excel file
      uiOutput("processed_file_table_header"),
      tableOutput("processed_file_table") # Display uploaded Excel file
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output, session) {

   observeEvent(input$display_metadata, {
         output$processed_file_table_header <- renderUI({h3("Metadata sheet (truncated)")})
         output$processed_file_table <- renderTable({
              if (input$specify_manually) {
                 req(input$input_cohort_1_row_1_1)
                 transform_df(processed_data())
               } else {
                 req(input$file1)
                 transform_df(read_excel(input$file1$datapath))
              }
         })
      })
     


   observeEvent(input$display_excel, {
      output$uploaded_file_table_header <- renderUI({h3("Uploaded metadata")})
      output$uploaded_file_table <- renderTable({
         uploaded_data() %>% select(all_of(input$select_columns))
      })
   })
  
  # Generate dynamic input fields for Versuchsbezeichnung and DataFile based on the number of cohorts
  observeEvent(input$generate_cohorts, {
    output$cohort_name_inputs <- renderUI({
      n_cohorts <- input$n_cohorts
      lapply(1:n_cohorts, function(cohort_num) {
        tagList(
          textInput(paste0("versuchsbezeichnung_", cohort_num), paste("Versuchsbezeichnung for Cohort", cohort_num), value = paste("Cohort", cohort_num)),
          textInput(paste0("datafile_", cohort_num), paste("DataFile for Cohort", cohort_num), value = paste("DataFile", cohort_num))
        )
      })
    })
  })
  
  # Generate dynamic inputs for the number of rows per cohort
  observeEvent(input$generate_cohorts, {
    output$dynamic_cohort_inputs <- renderUI({
      n_cohorts <- input$n_cohorts
      
      lapply(1:n_cohorts, function(cohort_num) {
        tagList(
          numericInput(paste0("n_rows_cohort", cohort_num), paste("How many rows for Cohort", cohort_num, "?"), value = 2, min = 1),
          actionButton(paste0("toggleCohort", cohort_num), paste("Toggle Cohort", cohort_num), class = paste0("cohort-btn-", cohort_num)),
          hidden(
            div(id = paste0("cohort_", cohort_num),
                uiOutput(paste0("cohort_inputs_", cohort_num))
            )
          )
        )
      })
    })
  })
  
  # Generate dynamic inputs for each cohort based on the number of rows specified and selected columns
  observeEvent(input$generate_cohorts, {
    lapply(1:input$n_cohorts, function(cohort_num) {
      observeEvent(input[[paste0("n_rows_cohort", cohort_num)]], {
        output[[paste0("cohort_inputs_", cohort_num)]] <- renderUI({
          n_rows <- input[[paste0("n_rows_cohort", cohort_num)]]
          selected_columns <- input$select_columns  # Get selected columns
          
          lapply(1:n_rows, function(row_num) {
            tagList(
              fluidRow(
                lapply(selected_columns, function(col) {
                  div(
                    class = paste0("text-input-", cohort_num),  # Apply cohort-specific class
                    textInput(paste0("input_cohort", cohort_num, "_row", row_num, "_", col), 
                              label = paste(col, "- Cohort", cohort_num, "Row", row_num), 
                              value = "")
                  )
                })
              ),
              hr()  # Add a horizontal rule to separate each row visually
            )
          })
        })
      })
    })
  })
  
  # Observe the cohort toggle buttons and toggle visibility
  observeEvent(input$generate_cohorts, {
    lapply(1:input$n_cohorts, function(cohort_num) {
      observeEvent(input[[paste0("toggleCohort", cohort_num)]], {
        toggle(paste0("cohort_", cohort_num))
      })
    })
  })

  uploaded_data <- reactive({
      req(input$file1) # ensure file has been uploaded
      df <- suppressWarnings(read_excel(input$file1$datapath, col_names = TRUE))
      df <- df %>% mutate(across(all_of(date_columns), ~ as.Date(., format = "%d/%m/%Y"))) %>%
         mutate(across(all_of(date_columns), ~format(., "%d/%m/%Y"))) %>%
         mutate(across(all_of(time_columns), ~format(as.POSIXct(., format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%H:%M:%S")))
      return(df)
  })
  
  # Reactive expression to process the Excel file or manually entered data
  processed_data <- reactive({
    if (!is.null(input$file1)) {
      # Step 1: Read the uploaded Excel file
      #df <- read_excel(input$file1$datapath, col_names = TRUE)

      df <- supressWarnings(read_excel(input$file1$datapath, col_names = TRUE))
      df <- df %>% mutate(across(all_of(date_columns), ~ as.Date(., format = "%d/%m/%Y"))) %>%
         mutate(across(all_of(date_columns), ~format(., "%d/%m/%Y"))) %>%
         mutate(across(all_of(time_columns), ~format(as.POSIXct(., format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%H:%M:%S")))


      
      # Step 2: Replace NA values with empty strings
      df <- df %>%
        mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

      # Step 3: Set column names from excel
      updateSelectInput(session, "select_columns", choices=names(df), selected=names(df))
      
    } else {
      # No file uploaded, create an empty dataframe to hold the results
      selected_columns <- input$select_columns
      df <- data.frame(matrix(ncol = length(selected_columns) + 2, nrow = 0))  # +2 for Versuchsbezeichnung and DataFile
      colnames(df) <- c("Versuchsbezeichnung", selected_columns, "DataFile")
      
      # Process manual input for each cohort and row
      for (cohort_num in 1:input$n_cohorts) {
        n_rows <- input[[paste0("n_rows_cohort", cohort_num)]]
        versuchsbezeichnung <- input[[paste0("versuchsbezeichnung_", cohort_num)]]  # Get the Versuchsbezeichnung name
        datafile <- input[[paste0("datafile_", cohort_num)]]  # Get the DataFile name
        for (row_num in 1:n_rows) {
          new_row <- sapply(selected_columns, function(col) {
            input[[paste0("input_cohort", cohort_num, "_row", row_num, "_", col)]]
          }, simplify = FALSE)
          new_row <- c(list(Versuchsbezeichnung = versuchsbezeichnung), new_row, list(DataFile = datafile))
          df <- rbind(df, as.data.frame(new_row, stringsAsFactors = FALSE))
        }
      }
    }
     
    colnames(df) <- selected_columns
    #output$status <- renderText("Processed data")
    return(df)

  })
  
  # Generate downloadable Excel file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("processed_data", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
       if (!input$specify_manually) {
      df <- supressWarnings(read_excel(input$file1$datapath, col_names = TRUE))
      df <- df %>% mutate(across(all_of(date_columns), ~ as.Date(., format = "%d/%m/%Y"))) %>%
         mutate(across(all_of(date_columns), ~format(., "%d/%m/%Y")))
         mutate(across(all_of(time_columns), ~format(as.POSIXct(., format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%H:%M:%S")))
         write_xlsx(df %>% select(all_of(input$select_columns)), file)
       }
    }
  )

 # Generate downloadable Excel file
  output$downloadMetadata <- downloadHandler(
    filename = function() {
      paste("processed_metadata", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      if (input$specify_manually) {
         write_xlsx(transform_df(processed_data()), file)
         output$status <- renderText("Converted metadata to metadata sheet")
      } else {
         write_xlsx(transform_df(read_excel(input$file1$datapath)), file)
      }
    }
  )
}

# Run the application on port 1339
shinyApp(ui = ui, server = server, options = list(port = 1339))
