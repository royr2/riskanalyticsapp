# Feature Selection Module

library(shiny)
library(dplyr)
library(caret)
library(DT)
library(ggplot2)

#' UI function for feature selection module
#'
#' @param id The namespace id for the module
#' @return A UI element
#' @export
feature_selection_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Feature Selection"),
    p("Select and prioritize the most relevant features for your model."),
    
    tabsetPanel(
      tabPanel("Filter Methods", 
               wellPanel(
                 h4("Filter-based Feature Selection"),
                 p("These methods evaluate features based on their statistical properties."),
                 
                 selectInput(ns("filter_method"), "Select method:",
                          choices = c("Correlation-based" = "correlation",
                                    "Chi-squared Test" = "chi_squared",
                                    "Information Gain" = "info_gain",
                                    "ANOVA" = "anova",
                                    "Variance Threshold" = "variance")),
                 
                 conditionalPanel(
                   condition = paste0("input['", ns("filter_method"), "'] == 'correlation'"),
                   ns = ns,
                   sliderInput(ns("correlation_threshold"), "Correlation threshold:",
                              min = 0, max = 1, value = 0.7, step = 0.05)
                 ),
                 
                 conditionalPanel(
                   condition = paste0("input['", ns("filter_method"), "'] == 'variance'"),
                   ns = ns,
                   sliderInput(ns("variance_threshold"), "Variance threshold:",
                              min = 0, max = 1, value = 0.1, step = 0.05)
                 ),
                 
                 actionButton(ns("apply_filter"), "Apply Filter Method", class = "btn-primary")
               ),
               
               hr(),
               
               h4("Filter Results"),
               plotOutput(ns("filter_plot")),
               DTOutput(ns("filter_results"))
      ),
      
      tabPanel("Wrapper Methods", 
               wellPanel(
                 h4("Wrapper-based Feature Selection"),
                 p("These methods evaluate features using a specific model and search strategy."),
                 
                 selectInput(ns("wrapper_method"), "Select method:",
                          choices = c("Recursive Feature Elimination" = "rfe",
                                    "Forward Selection" = "forward",
                                    "Backward Elimination" = "backward")),
                 
                 selectInput(ns("wrapper_model"), "Base model for selection:",
                          choices = c("Linear Model" = "lm",
                                    "Random Forest" = "rf",
                                    "Gradient Boosting" = "gbm")),
                 
                 numericInput(ns("num_features"), "Number of features to select:",
                            min = 1, value = 5),
                 
                 actionButton(ns("apply_wrapper"), "Apply Wrapper Method", class = "btn-primary")
               ),
               
               hr(),
               
               h4("Wrapper Results"),
               plotOutput(ns("wrapper_plot")),
               verbatimTextOutput(ns("wrapper_details")),
               DTOutput(ns("wrapper_results"))
      ),
      
      tabPanel("Embedded Methods", 
               wellPanel(
                 h4("Embedded Feature Selection"),
                 p("These methods incorporate feature selection as part of the model building process."),
                 
                 selectInput(ns("embedded_method"), "Select method:",
                          choices = c("LASSO Regularization" = "lasso",
                                    "Random Forest Importance" = "rf_importance",
                                    "Gradient Boosting Importance" = "gbm_importance")),
                 
                 conditionalPanel(
                   condition = paste0("input['", ns("embedded_method"), "'] == 'lasso'"),
                   ns = ns,
                   sliderInput(ns("alpha"), "Alpha (L1 penalty):",
                              min = 0, max = 1, value = 0.5, step = 0.05)
                 ),
                 
                 actionButton(ns("apply_embedded"), "Apply Embedded Method", class = "btn-primary")
               ),
               
               hr(),
               
               h4("Embedded Results"),
               plotOutput(ns("embedded_plot")),
               DTOutput(ns("embedded_results"))
      )
    ),
    
    hr(),
    
    wellPanel(
      h4("Final Feature Selection"),
      p("Review and confirm the features you want to use in your model."),
      
      verbatimTextOutput(ns("selected_features_summary")),
      
      DT::dataTableOutput(ns("feature_table")),
      
      actionButton(ns("use_all"), "Use All Features"),
      actionButton(ns("use_selected"), "Use Selected Features"),
      
      hr(),
      
      fluidRow(
        column(6,
               actionButton(ns("skip_feature_selection"), "Skip Feature Selection", 
                          class = "btn-warning",
                          title = "Use all available features and proceed directly")
        ),
        column(6,
               actionButton(ns("confirm_features"), "Confirm Selected Features", 
                          class = "btn-success")
        )
      )
    )
  )
}

#' Server function for feature selection module
#'
#' @param id The namespace id for the module
#' @param column_data Reactive expression from the column selection module
#' @return A reactive expression containing the selected features data
#' @export
feature_selection_Server <- function(id, column_data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Reactive values to store results from different methods
    filter_features <- reactiveVal(NULL)
    wrapper_features <- reactiveVal(NULL)
    embedded_features <- reactiveVal(NULL)
    
    # Final selected features
    final_features <- reactiveVal(NULL)
    
    # Extract data from the column selection module
    selected_data <- reactive({
      req(column_data())
      column_data()$data
    })
    
    target_column <- reactive({
      req(column_data())
      column_data()$target_column
    })
    
    predictor_columns <- reactive({
      req(column_data())
      column_data()$predictor_columns
    })
    
    problem_type <- reactive({
      req(column_data())
      column_data()$problem_type
    })
    
    # Apply filter-based feature selection
    observeEvent(input$apply_filter, {
      req(selected_data(), target_column(), predictor_columns())
      
      withProgress(message = "Applying filter method...", value = 0.1, {
        
        data <- selected_data()
        target <- target_column()
        predictors <- predictor_columns()
        
        # Different methods based on problem type and selected filter
        if (input$filter_method == "correlation") {
          # Correlation-based feature selection
          if (problem_type() == "regression") {
            # For regression - correlation with target
            cor_matrix <- cor(data[, c(target, predictors)], use = "pairwise.complete.obs")
            feature_importance <- abs(cor_matrix[target, predictors, drop = FALSE])
            
            # Get features above threshold
            selected <- names(feature_importance)[feature_importance > input$correlation_threshold]
            feature_table <- data.frame(
              Feature = predictors,
              Importance = as.numeric(feature_importance),
              Selected = predictors %in% selected
            )
            
            # Sort by importance
            feature_table <- feature_table[order(-feature_table$Importance), ]
            
            filter_features(list(
              features = selected,
              importance = feature_table
            ))
            
            # Create correlation plot
            output$filter_plot <- renderPlot({
              ggplot(feature_table, aes(x = reorder(Feature, Importance), y = Importance)) +
                geom_bar(stat = "identity", fill = "steelblue") +
                geom_hline(yintercept = input$correlation_threshold, linetype = "dashed", color = "red") +
                labs(x = "Features", y = "Correlation with Target", title = "Feature Correlation") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            })
          } else {
            # For classification - handle categorical variables differently
            # This is a simplified approach - for categorical target with numeric predictors
            feature_table <- data.frame(Feature = character(), Importance = numeric(), Selected = logical())
            
            for (pred in predictors) {
              if (is.numeric(data[[pred]])) {
                # Use ANOVA for categorical target and numeric predictor
                formula_str <- paste(pred, "~", target)
                model <- aov(as.formula(formula_str), data = data)
                importance <- summary(model)[[1]]["F value"][[1]][1]
                
                feature_table <- rbind(feature_table, data.frame(
                  Feature = pred,
                  Importance = importance,
                  Selected = !is.na(importance) && importance > 2 # arbitrary threshold
                ))
              }
            }
            
            if (nrow(feature_table) > 0) {
              # Sort by importance
              feature_table <- feature_table[order(-feature_table$Importance), ]
              selected <- as.character(feature_table$Feature[feature_table$Selected])
              
              filter_features(list(
                features = selected,
                importance = feature_table
              ))
              
              # Create importance plot
              output$filter_plot <- renderPlot({
                ggplot(feature_table, aes(x = reorder(Feature, Importance), y = Importance)) +
                  geom_bar(stat = "identity", fill = "steelblue") +
                  labs(x = "Features", y = "F Value (ANOVA)", title = "Feature Importance (ANOVA)") +
                  theme_minimal() +
                  theme(axis.text.x = element_text(angle = 45, hjust = 1))
              })
            } else {
              filter_features(list(features = predictors, importance = NULL))
              output$filter_plot <- renderPlot({ NULL })
            }
          }
        } else if (input$filter_method == "variance") {
          # Variance threshold method
          variances <- sapply(data[, predictors, drop = FALSE], var, na.rm = TRUE)
          max_var <- max(variances, na.rm = TRUE)
          norm_var <- variances / max_var  # Normalize to 0-1
          
          selected <- names(norm_var)[norm_var > input$variance_threshold]
          
          feature_table <- data.frame(
            Feature = names(variances),
            Importance = as.numeric(norm_var),
            Selected = names(variances) %in% selected
          )
          
          # Sort by importance
          feature_table <- feature_table[order(-feature_table$Importance), ]
          
          filter_features(list(
            features = selected,
            importance = feature_table
          ))
          
          # Create variance plot
          output$filter_plot <- renderPlot({
            ggplot(feature_table, aes(x = reorder(Feature, Importance), y = Importance)) +
              geom_bar(stat = "identity", fill = "steelblue") +
              geom_hline(yintercept = input$variance_threshold, linetype = "dashed", color = "red") +
              labs(x = "Features", y = "Normalized Variance", title = "Feature Variance") +
              theme_minimal() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          })
        } else {
          # For other methods, we would implement specific code here
          # For now, return all features as a placeholder
          filter_features(list(
            features = predictors,
            importance = data.frame(Feature = predictors, Importance = 1, Selected = TRUE)
          ))
          
          output$filter_plot <- renderPlot({ 
            plot(1, type = "n", xlab = "", ylab = "", 
                main = paste("Feature selection method", input$filter_method, "not yet implemented"))
            text(1, 1, "Coming soon")
          })
        }
        
        # Display filter results
        output$filter_results <- renderDT({
          req(filter_features())
          importance_df <- filter_features()$importance
          if (!is.null(importance_df)) {
            datatable(importance_df, options = list(pageLength = 10))
          }
        })
        
        # Update final features
        if (!is.null(filter_features()) && length(filter_features()$features) > 0) {
          final_features(filter_features()$features)
        } else {
          final_features(predictor_columns())
        }
        
        # Update feature summary
        update_feature_summary()
      })
    })
    
    # Apply wrapper-based feature selection
    observeEvent(input$apply_wrapper, {
      req(selected_data(), target_column(), predictor_columns())
      
      withProgress(message = "Applying wrapper method (this may take time)...", value = 0.1, {
        # In a real application, this would be more sophisticated
        # For now, just a simple demonstration with a subset of features
        
        # RFE example (would normally be more complex)
        n_features <- min(input$num_features, length(predictor_columns()))
        selected <- sample(predictor_columns(), n_features)
        
        feature_table <- data.frame(
          Feature = predictor_columns(),
          Selected = predictor_columns() %in% selected
        )
        
        wrapper_features(list(
          features = selected,
          results = feature_table
        ))
        
        # Basic plot for demonstration
        output$wrapper_plot <- renderPlot({
          plot(1:10, type = "o", xlab = "Number of Features", ylab = "Performance", 
              main = "Feature Selection Performance")
          points(n_features, 0.7, col = "red", pch = 19)
        })
        
        output$wrapper_details <- renderPrint({
          paste("Selected", n_features, "features using", input$wrapper_method, 
                "with a", input$wrapper_model, "base model")
        })
        
        output$wrapper_results <- renderDT({
          req(wrapper_features())
          datatable(wrapper_features()$results, options = list(pageLength = 10))
        })
        
        # Update final features
        final_features(selected)
        
        # Update feature summary
        update_feature_summary()
      })
    })
    
    # Apply embedded feature selection
    observeEvent(input$apply_embedded, {
      req(selected_data(), target_column(), predictor_columns())
      
      withProgress(message = "Applying embedded method...", value = 0.1, {
        # In a real application, this would be more sophisticated
        # For now, just a simple demonstration
        
        # Random example weights (would normally come from actual model)
        set.seed(123)
        weights <- runif(length(predictor_columns()), 0, 1)
        
        feature_table <- data.frame(
          Feature = predictor_columns(),
          Importance = weights,
          Selected = weights > 0.5
        )
        
        feature_table <- feature_table[order(-feature_table$Importance), ]
        
        selected <- as.character(feature_table$Feature[feature_table$Selected])
        
        embedded_features(list(
          features = selected,
          importance = feature_table
        ))
        
        # Create importance plot
        output$embedded_plot <- renderPlot({
          req(embedded_features())
          df <- embedded_features()$importance
          
          ggplot(df, aes(x = reorder(Feature, Importance), y = Importance)) +
            geom_bar(stat = "identity", fill = "steelblue") +
            geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
            labs(x = "Features", y = "Importance", 
                title = paste("Feature Importance from", input$embedded_method)) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        })
        
        output$embedded_results <- renderDT({
          req(embedded_features())
          datatable(embedded_features()$importance, options = list(pageLength = 10))
        })
        
        # Update final features
        final_features(selected)
        
        # Update feature summary
        update_feature_summary()
      })
    })
    
    # Use all features
    observeEvent(input$use_all, {
      req(predictor_columns())
      final_features(predictor_columns())
      update_feature_summary()
    })
    
    # Update the feature table for selection
    output$feature_table <- renderDT({
      req(selected_data())
      
      # Create a table with all predictors and their current selection status
      feature_df <- data.frame(
        Feature = predictor_columns(),
        Selected = predictor_columns() %in% (final_features() %||% predictor_columns())
      )
      
      datatable(feature_df, 
               options = list(pageLength = 10),
               selection = "multiple")
    })
    
    # Use selected features from the table
    observeEvent(input$use_selected, {
      req(input$feature_table_rows_selected)
      
      # Get the selected rows from the table
      selected_rows <- input$feature_table_rows_selected
      
      # Update final features based on the selection
      features_df <- data.frame(
        Feature = predictor_columns(),
        Selected = FALSE
      )
      features_df$Selected[selected_rows] <- TRUE
      
      selected <- as.character(features_df$Feature[features_df$Selected])
      final_features(selected)
      
      update_feature_summary()
    })
    
    # Function to update feature summary
    update_feature_summary <- function() {
      output$selected_features_summary <- renderText({
        req(final_features())
        
        paste("Selected", length(final_features()), "features out of", 
             length(predictor_columns()), "total features:",
             paste(final_features(), collapse = ", "))
      })
    }
    
    # Skip feature selection - use all features
    observeEvent(input$skip_feature_selection, {
      req(column_data())
      
      # Use all predictor columns
      final_features(predictor_columns())
      
      # Show notification about skipping
      showNotification(
        paste("✅ Skipped feature selection. Using all", length(predictor_columns()), "features."),
        type = "message",
        duration = 3
      )
      
      # Automatically trigger the feature selection result
      session$userData$skip_triggered <- TRUE
      
      update_feature_summary()
    })
    
    # Return the final selected features when confirmed
    feature_selection_result <- eventReactive(c(input$confirm_features, input$skip_feature_selection), {
      req(selected_data(), target_column())
      
      # Use final_features if available, otherwise use all predictor columns
      features_to_use <- if(length(final_features()) > 0) final_features() else predictor_columns()
      
      # Get data with only selected features and target
      df <- selected_data()
      selected_cols <- c(target_column(), features_to_use)
      result_data <- df[, selected_cols, drop = FALSE]
      
      list(
        data = result_data,
        target_column = target_column(),
        predictor_columns = features_to_use,
        problem_type = problem_type()
      )
    })
    
    # Also provide a reactive that passes through data even without confirmation
    # This allows the next module to receive data immediately
    data_passthrough <- reactive({
      if(!is.null(column_data()) && !is.null(column_data()$data)) {
        # If column data is available, pass it through
        return(column_data())
      } else {
        return(NULL)
      }
    })
    
    # Return both the confirmed result and the passthrough
    return(reactive({
      # Check if either button was clicked
      if((length(reactiveValuesToList(input)) > 0 && !is.null(input$confirm_features) && input$confirm_features > 0) ||
         (length(reactiveValuesToList(input)) > 0 && !is.null(input$skip_feature_selection) && input$skip_feature_selection > 0)) {
        # If features have been confirmed or skipped, return the processed result
        feature_selection_result()
      } else {
        # Otherwise, pass through the column data
        data_passthrough()
      }
    }))
  })
}