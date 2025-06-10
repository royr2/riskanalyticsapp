# Data preprocessing module

library(shiny)
library(dplyr)
library(tidyr)
library(DT)

#' UI function for data preprocessing module
#'
#' @param id The namespace id for the module
#' @return A UI element
#' @export
data_preprocessing_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Data Preprocessing"),
    p("Clean and preprocess your data before analysis."),
    
    tabsetPanel(
      tabPanel("Missing Values",
               h4("Handle Missing Values"),
               radioButtons(ns("na_method"), "Method:",
                           choices = c("Remove rows" = "remove",
                                     "Impute with mean/mode" = "impute"),
                           selected = "remove"),
               actionButton(ns("handle_na"), "Apply", class = "btn-primary")
      ),
      
      tabPanel("Data Types",
               h4("Convert Data Types"),
               uiOutput(ns("column_type_selection")),
               actionButton(ns("convert_types"), "Convert", class = "btn-primary")
      ),
      
      tabPanel("Normalization",
               h4("Normalize Numeric Variables"),
               checkboxInput(ns("normalize"), "Normalize numeric columns", value = FALSE),
               selectInput(ns("normalize_method"), "Method:",
                         choices = c("Z-score standardization" = "zscore",
                                   "Min-max scaling" = "minmax"),
                         selected = "zscore"),
               actionButton(ns("apply_normalization"), "Apply", class = "btn-primary")
      ),
      
      tabPanel("Feature Engineering",
               h4("Basic Feature Engineering"),
               uiOutput(ns("feature_engineering_ui")),
               actionButton(ns("apply_fe"), "Apply", class = "btn-primary")
      )
    ),
    
    hr(),
    
    h4("Preprocessing Status"),
    verbatimTextOutput(ns("preprocessing_status")),
    
    hr(),
    
    h4("Data Preview"),
    DTOutput(ns("processed_data_preview")),
    
    actionButton(ns("finalize_preprocessing"), "Finalize and Continue", 
                class = "btn-success", 
                style = "margin-top: 20px;")
  )
}

#' Server function for data preprocessing module
#'
#' @param id The namespace id for the module
#' @param data_input Reactive expression that returns the imported data
#' @return A reactive expression containing the preprocessed data
#' @export
data_preprocessing_Server <- function(id, data_input) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Reactive value to store the current state of data
    current_data <- reactiveVal(NULL)
    
    # Track preprocessing steps
    preprocessing_steps <- reactiveVal(character(0))
    
    # Initialize current data when input data changes
    observe({
      req(data_input())
      current_data(data_input())
      preprocessing_steps(c("Data imported successfully"))
    })
    
    # Generate UI for column type selection
    output$column_type_selection <- renderUI({
      req(current_data())
      data <- current_data()
      
      column_types <- sapply(data, class)
      
      tagList(
        p("Select columns to convert:"),
        lapply(names(data), function(col) {
          div(
            checkboxInput(ns(paste0("col_", col)), col, value = FALSE),
            conditionalPanel(
              condition = paste0("input['", ns(paste0("col_", col)), "'] == true"),
              selectInput(ns(paste0("type_", col)), "Convert to:",
                        choices = c("Numeric" = "numeric",
                                  "Factor" = "factor",
                                  "Date" = "date",
                                  "Text" = "character"),
                        selected = column_types[col])
            )
          )
        })
      )
    })
    
    # Generate UI for feature engineering
    output$feature_engineering_ui <- renderUI({
      req(current_data())
      data <- current_data()
      
      numeric_cols <- names(data)[sapply(data, is.numeric)]
      categorical_cols <- names(data)[sapply(data, is.factor) | sapply(data, is.character)]
      
      tagList(
        h5("Create interactions between variables"),
        selectInput(ns("fe_var1"), "Variable 1:", choices = names(data)),
        selectInput(ns("fe_var2"), "Variable 2:", choices = names(data)),
        selectInput(ns("fe_operation"), "Operation:",
                  choices = c("Interaction (×)" = "interaction",
                            "Addition (+)" = "addition",
                            "Subtraction (-)" = "subtraction",
                            "Division (/)" = "division"),
                  selected = "interaction"),
        
        conditionalPanel(
          condition = paste0("input['", ns("fe_var1"), "'] != '' && input['", ns("fe_var2"), "'] != ''"),
          textInput(ns("new_feature_name"), "New feature name:",
                  value = "new_feature")
        ),
        
        hr(),
        
        h5("One-hot encode categorical variables"),
        checkboxGroupInput(ns("one_hot_cols"), "Select categorical variables to encode:",
                         choices = categorical_cols)
      )
    })
    
    # Handle missing values
    observeEvent(input$handle_na, {
      req(current_data())
      data <- current_data()
      
      withProgress(
        message = "Handling missing values...",
        value = 0.5, {
          
          if (input$na_method == "remove") {
            data <- data %>% drop_na()
            preprocessing_steps(c(preprocessing_steps(), "Removed rows with missing values"))
          } else if (input$na_method == "impute") {
            # Impute numeric columns with mean
            for (col in names(data)) {
              if (is.numeric(data[[col]]) && any(is.na(data[[col]]))) {
                data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
              } else if ((is.factor(data[[col]]) || is.character(data[[col]])) && any(is.na(data[[col]]))) {
                # For categorical, impute with mode
                mode_val <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
                data[[col]][is.na(data[[col]])] <- mode_val
              }
            }
            preprocessing_steps(c(preprocessing_steps(), "Imputed missing values with mean/mode"))
          }
          
          current_data(data)
        }
      )
    })
    
    # Convert data types
    observeEvent(input$convert_types, {
      req(current_data())
      data <- current_data()
      
      withProgress(
        message = "Converting data types...",
        value = 0.5, {
          
          conversions <- c()
          
          for (col in names(data)) {
            checkbox_id <- paste0("col_", col)
            if (isTRUE(input[[checkbox_id]])) {
              type_id <- paste0("type_", col)
              new_type <- input[[type_id]]
              
              if (new_type == "numeric") {
                data[[col]] <- as.numeric(data[[col]])
                conversions <- c(conversions, paste0(col, " to numeric"))
              } else if (new_type == "factor") {
                data[[col]] <- as.factor(data[[col]])
                conversions <- c(conversions, paste0(col, " to factor"))
              } else if (new_type == "date") {
                # Try to convert to date - may need more sophisticated handling
                tryCatch({
                  data[[col]] <- as.Date(data[[col]])
                  conversions <- c(conversions, paste0(col, " to date"))
                }, error = function(e) {
                  showNotification(paste("Failed to convert", col, "to date"), type = "error")
                })
              } else if (new_type == "character") {
                data[[col]] <- as.character(data[[col]])
                conversions <- c(conversions, paste0(col, " to character"))
              }
            }
          }
          
          if (length(conversions) > 0) {
            preprocessing_steps(c(preprocessing_steps(), 
                                paste("Converted:", paste(conversions, collapse = ", "))))
          }
          
          current_data(data)
        }
      )
    })
    
    # Apply normalization
    observeEvent(input$apply_normalization, {
      req(current_data(), input$normalize)
      
      if (input$normalize) {
        data <- current_data()
        
        withProgress(
          message = "Normalizing numeric columns...",
          value = 0.5, {
            
            # Identify numeric columns
            num_cols <- sapply(data, is.numeric)
            
            if (sum(num_cols) > 0) {
              if (input$normalize_method == "zscore") {
                # Z-score standardization
                data[num_cols] <- as.data.frame(scale(data[num_cols]))
                preprocessing_steps(c(preprocessing_steps(), "Applied Z-score standardization to numeric columns"))
              } else if (input$normalize_method == "minmax") {
                # Min-max scaling
                for (col in names(data)[num_cols]) {
                  min_val <- min(data[[col]], na.rm = TRUE)
                  max_val <- max(data[[col]], na.rm = TRUE)
                  data[[col]] <- (data[[col]] - min_val) / (max_val - min_val)
                }
                preprocessing_steps(c(preprocessing_steps(), "Applied min-max scaling to numeric columns"))
              }
            }
            
            current_data(data)
          }
        )
      }
    })
    
    # Apply feature engineering
    observeEvent(input$apply_fe, {
      req(current_data())
      data <- current_data()
      
      withProgress(
        message = "Applying feature engineering...",
        value = 0.5, {
          
          # Create interaction/operation between variables
          if (!is.null(input$fe_var1) && !is.null(input$fe_var2) && !is.null(input$new_feature_name)) {
            var1 <- data[[input$fe_var1]]
            var2 <- data[[input$fe_var2]]
            
            if (input$fe_operation == "interaction") {
              # For categorical variables, create interaction term
              if ((is.factor(var1) || is.character(var1)) && 
                  (is.factor(var2) || is.character(var2))) {
                data[[input$new_feature_name]] <- as.factor(paste(var1, var2, sep = "_"))
              } else if (is.numeric(var1) && is.numeric(var2)) {
                # For numeric variables, multiply
                data[[input$new_feature_name]] <- var1 * var2
              }
              preprocessing_steps(c(preprocessing_steps(), 
                                  paste0("Created interaction between ", 
                                         input$fe_var1, " and ", input$fe_var2)))
            } else if (input$fe_operation == "addition" && is.numeric(var1) && is.numeric(var2)) {
              data[[input$new_feature_name]] <- var1 + var2
              preprocessing_steps(c(preprocessing_steps(), 
                                  paste0("Created sum of ", 
                                         input$fe_var1, " and ", input$fe_var2)))
            } else if (input$fe_operation == "subtraction" && is.numeric(var1) && is.numeric(var2)) {
              data[[input$new_feature_name]] <- var1 - var2
              preprocessing_steps(c(preprocessing_steps(), 
                                  paste0("Created difference between ", 
                                         input$fe_var1, " and ", input$fe_var2)))
            } else if (input$fe_operation == "division" && is.numeric(var1) && is.numeric(var2)) {
              data[[input$new_feature_name]] <- var1 / var2
              preprocessing_steps(c(preprocessing_steps(), 
                                  paste0("Created ratio of ", 
                                         input$fe_var1, " to ", input$fe_var2)))
            }
          }
          
          # One-hot encoding for selected categorical variables
          if (!is.null(input$one_hot_cols) && length(input$one_hot_cols) > 0) {
            for (col in input$one_hot_cols) {
              if (is.factor(data[[col]]) || is.character(data[[col]])) {
                # Create dummy variables
                dummies <- model.matrix(~ 0 + data[[col]])
                colnames(dummies) <- paste0(col, "_", levels(as.factor(data[[col]])))
                
                # Bind to original data
                data <- cbind(data, as.data.frame(dummies))
              }
            }
            preprocessing_steps(c(preprocessing_steps(), 
                                paste0("Applied one-hot encoding to: ", 
                                       paste(input$one_hot_cols, collapse = ", "))))
          }
          
          current_data(data)
        }
      )
    })
    
    # Update preprocessing status
    output$preprocessing_status <- renderText({
      req(preprocessing_steps())
      paste(preprocessing_steps(), collapse = "\n")
    })
    
    # Show processed data preview
    output$processed_data_preview <- renderDT({
      req(current_data())
      datatable(head(current_data(), 10), 
               options = list(scrollX = TRUE, 
                              dom = 'ftip',
                              pageLength = 5),
               rownames = FALSE)
    })
    
    # Return the preprocessed data when finalized
    finalized_data <- eventReactive(input$finalize_preprocessing, {
      req(current_data())
      return(current_data())
    })
    
    return(finalized_data)
  })
}