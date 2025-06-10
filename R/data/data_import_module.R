library(shiny)
library(DT)

#' UI function for data import module
#'
#' @param id The namespace id for the module
#' @return A UI element
#' @export
data_import_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Data Import"),
    p("Upload a CSV file to begin your analysis."),
    
    fileInput(ns("file"), "Upload CSV File", 
              accept = c("text/csv", 
                         "text/comma-separated-values,text/plain", 
                         ".csv")),
    
    checkboxInput(ns("header"), "Header", TRUE),
    
    selectInput(ns("sep"), "Separator",
                choices = c(Comma = ",",
                           Semicolon = ";",
                           Tab = "\t"),
                selected = ","),
    
    selectInput(ns("quote"), "Quote",
               choices = c(None = "",
                          "Double Quote" = '"',
                          "Single Quote" = "'"),
               selected = '"'),
               
    tags$hr(),
    
    actionButton(ns("import"), "Import Data", 
                class = "btn-primary"),
    
    tags$hr(),
    
    conditionalPanel(
      condition = "input.import > 0", 
      ns = ns,
      h4("Data Preview"),
      DTOutput(ns("data_preview")),
      
      uiOutput(ns("data_summary"))
    )
  )
}

#' Server function for data import module
#'
#' @param id The namespace id for the module
#' @return A reactive expression containing the imported data
#' @export
data_import_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive value to store the imported data
    imported_data <- reactiveVal(NULL)
    
    # Handle import button click
    observeEvent(input$import, {
      req(input$file)
      
      # Show loading message
      withProgress(
        message = "Importing data...",
        value = 0.5, {
          
          tryCatch({
            data <- read.csv(input$file$datapath, 
                           header = input$header,
                           sep = input$sep,
                           quote = input$quote)
            
            # Store the imported data
            imported_data(data)
            
            # Update the preview
            output$data_preview <- renderDT({
              req(imported_data())
              datatable(head(imported_data(), 10), 
                       options = list(scrollX = TRUE, 
                                      dom = 'ftip',
                                      pageLength = 5),
                       rownames = FALSE)
            })
            
            # Generate data summary
            output$data_summary <- renderUI({
              req(imported_data())
              data <- imported_data()
              
              tagList(
                h4("Data Summary"),
                p(paste("Rows:", nrow(data))),
                p(paste("Columns:", ncol(data))),
                p(paste("Column types:", paste(sapply(data, class), collapse = ", ")))
              )
            })
            
          }, error = function(e) {
            showNotification(paste("Error importing data:", e$message), type = "error")
          })
        }
      )
    })
    
    # Return the reactive containing the imported data
    return(imported_data)
  })
}