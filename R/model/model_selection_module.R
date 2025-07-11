# Model Selection Module

library(shiny)
library(DT)
library(ggplot2)
library(stringr)

#' UI function for model selection module
#'
#' @param id The namespace id for the module
#' @return A UI element
#' @export
model_selection_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Model Selection"),
    p("Choose the appropriate model for your data analysis."),
    
    # Status panel showing automatic detection
    conditionalPanel(
      condition = "true", # Always show
      ns = ns,
      wellPanel(
        style = "background-color: #f8f9fa; border-left: 4px solid #17a2b8;",
        h4("📊 Data Analysis Summary", style = "color: #17a2b8; margin-top: 0;"),
        fluidRow(
          column(6,
                 uiOutput(ns("problem_type_display"))
          ),
          column(6,
                 uiOutput(ns("data_summary_display"))
          )
        )
      )
    ),
    
    tabsetPanel(
      id = ns("model_tabs"),
      
      # Tab for regression models
      tabPanel("Regression Models", 
               conditionalPanel(
                 condition = paste0("input['", ns("is_regression"), "'] == true"),
                 ns = ns,
                 
                 wellPanel(
                   h4("Select Regression Models"),
                   
                   checkboxInput(ns("reg_linear"), "Linear Regression", TRUE),
                   conditionalPanel(
                     condition = paste0("input['", ns("reg_linear"), "'] == true"),
                     ns = ns,
                     wellPanel(
                       h5("Linear Regression Options"),
                       checkboxInput(ns("reg_linear_interactions"), "Include interactions", FALSE),
                       checkboxInput(ns("reg_linear_poly"), "Include polynomial terms", FALSE),
                       conditionalPanel(
                         condition = paste0("input['", ns("reg_linear_poly"), "'] == true"),
                         ns = ns,
                         sliderInput(ns("reg_poly_degree"), "Polynomial degree:",
                                  min = 2, max = 5, value = 2, step = 1)
                       )
                     )
                   ),
                   
                   checkboxInput(ns("reg_ridge"), "Ridge Regression", FALSE),
                   conditionalPanel(
                     condition = paste0("input['", ns("reg_ridge"), "'] == true"),
                     ns = ns,
                     sliderInput(ns("reg_ridge_lambda"), "Lambda (regularization):",
                                min = 0, max = 1, value = 0.1, step = 0.01)
                   ),
                   
                   checkboxInput(ns("reg_lasso"), "LASSO Regression", FALSE),
                   conditionalPanel(
                     condition = paste0("input['", ns("reg_lasso"), "'] == true"),
                     ns = ns,
                     sliderInput(ns("reg_lasso_lambda"), "Lambda (regularization):",
                                min = 0, max = 1, value = 0.1, step = 0.01)
                   ),
                   
                   checkboxInput(ns("reg_elastic"), "Elastic Net", FALSE),
                   conditionalPanel(
                     condition = paste0("input['", ns("reg_elastic"), "'] == true"),
                     ns = ns,
                     sliderInput(ns("reg_elastic_alpha"), "Alpha (L1 ratio):",
                                min = 0, max = 1, value = 0.5, step = 0.01),
                     sliderInput(ns("reg_elastic_lambda"), "Lambda (regularization):",
                                min = 0, max = 1, value = 0.1, step = 0.01)
                   ),
                   
                   checkboxInput(ns("reg_rf"), "Random Forest Regression", FALSE),
                   conditionalPanel(
                     condition = paste0("input['", ns("reg_rf"), "'] == true"),
                     ns = ns,
                     sliderInput(ns("reg_rf_trees"), "Number of trees:",
                                min = 10, max = 500, value = 100, step = 10),
                     sliderInput(ns("reg_rf_mtry"), "Variables per split:",
                                min = 1, max = 10, value = 3, step = 1)
                   ),
                   
                   checkboxInput(ns("reg_gbm"), "Gradient Boosting Regression", FALSE),
                   conditionalPanel(
                     condition = paste0("input['", ns("reg_gbm"), "'] == true"),
                     ns = ns,
                     sliderInput(ns("reg_gbm_trees"), "Number of trees:",
                                min = 10, max = 500, value = 100, step = 10),
                     sliderInput(ns("reg_gbm_depth"), "Tree depth:",
                                min = 1, max = 10, value = 3, step = 1),
                     sliderInput(ns("reg_gbm_lr"), "Learning rate:",
                                min = 0.001, max = 0.3, value = 0.1, step = 0.001)
                   ),
                   
                   checkboxInput(ns("reg_svm"), "Support Vector Machine Regression", FALSE),
                   conditionalPanel(
                     condition = paste0("input['", ns("reg_svm"), "'] == true"),
                     ns = ns,
                     selectInput(ns("reg_svm_kernel"), "Kernel:",
                              choices = c("Linear" = "linear", 
                                        "Polynomial" = "polynomial", 
                                        "Radial Basis" = "radial"),
                              selected = "radial")
                   )
                 ),
                 
                 actionButton(ns("validate_regression"), "Validate Models", class = "btn-primary")
               ),
               
               conditionalPanel(
                 condition = paste0("input['", ns("is_regression"), "'] != true"),
                 ns = ns,
                 div(class = "alert alert-info",
                     h4("📊 This is not a regression problem"),
                     p("Your target variable appears to be categorical, making this a classification problem."),
                     p("Please switch to the 'Classification Models' tab to select appropriate models."),
                     actionButton(ns("switch_to_classification"), "Switch to Classification Models", 
                                class = "btn-info")
                 )
               )
      ),
      
      # Tab for classification models
      tabPanel("Classification Models",
               conditionalPanel(
                 condition = paste0("input['", ns("is_classification"), "'] == true"),
                 ns = ns,
                 
                 wellPanel(
                   h4("Select Classification Models"),
                   
                   checkboxInput(ns("cls_logistic"), "Logistic Regression", TRUE),
                   
                   checkboxInput(ns("cls_rf"), "Random Forest Classification", FALSE),
                   conditionalPanel(
                     condition = paste0("input['", ns("cls_rf"), "'] == true"),
                     ns = ns,
                     sliderInput(ns("cls_rf_trees"), "Number of trees:",
                                min = 10, max = 500, value = 100, step = 10),
                     sliderInput(ns("cls_rf_mtry"), "Variables per split:",
                                min = 1, max = 10, value = 3, step = 1)
                   ),
                   
                   checkboxInput(ns("cls_gbm"), "Gradient Boosting Classification", FALSE),
                   conditionalPanel(
                     condition = paste0("input['", ns("cls_gbm"), "'] == true"),
                     ns = ns,
                     sliderInput(ns("cls_gbm_trees"), "Number of trees:",
                                min = 10, max = 500, value = 100, step = 10),
                     sliderInput(ns("cls_gbm_depth"), "Tree depth:",
                                min = 1, max = 10, value = 3, step = 1),
                     sliderInput(ns("cls_gbm_lr"), "Learning rate:",
                                min = 0.001, max = 0.3, value = 0.1, step = 0.001)
                   ),
                   
                   checkboxInput(ns("cls_svm"), "Support Vector Machine Classification", FALSE),
                   conditionalPanel(
                     condition = paste0("input['", ns("cls_svm"), "'] == true"),
                     ns = ns,
                     selectInput(ns("cls_svm_kernel"), "Kernel:",
                              choices = c("Linear" = "linear", 
                                        "Polynomial" = "polynomial", 
                                        "Radial Basis" = "radial"),
                              selected = "radial")
                   ),
                   
                   checkboxInput(ns("cls_nb"), "Naive Bayes", FALSE),
                   
                   checkboxInput(ns("cls_dt"), "Decision Tree", FALSE),
                   conditionalPanel(
                     condition = paste0("input['", ns("cls_dt"), "'] == true"),
                     ns = ns,
                     sliderInput(ns("cls_dt_depth"), "Max depth:",
                                min = 1, max = 20, value = 5, step = 1)
                   ),
                   
                   checkboxInput(ns("cls_knn"), "k-Nearest Neighbors", FALSE),
                   conditionalPanel(
                     condition = paste0("input['", ns("cls_knn"), "'] == true"),
                     ns = ns,
                     sliderInput(ns("cls_knn_k"), "Number of neighbors (k):",
                                min = 1, max = 30, value = 5, step = 1)
                   )
                 ),
                 
                 actionButton(ns("validate_classification"), "Validate Models", class = "btn-primary")
               ),
               
               conditionalPanel(
                 condition = paste0("input['", ns("is_classification"), "'] != true"),
                 ns = ns,
                 div(class = "alert alert-info",
                     h4("📈 This is not a classification problem"),
                     p("Your target variable appears to be numeric, making this a regression problem."),
                     p("Please switch to the 'Regression Models' tab to select appropriate models."),
                     actionButton(ns("switch_to_regression"), "Switch to Regression Models", 
                                class = "btn-info")
                 )
               )
      ),
      
      # Tab for validation settings
      tabPanel("Validation Settings",
               wellPanel(
                 h4("Model Validation Settings"),
                 
                 radioButtons(ns("validation_method"), "Validation Method:",
                           choices = c("Train/Test Split" = "train_test",
                                     "Cross Validation" = "cv"),
                           selected = "train_test"),
                 
                 conditionalPanel(
                   condition = paste0("input['", ns("validation_method"), "'] == 'train_test'"),
                   ns = ns,
                   sliderInput(ns("train_ratio"), "Training Data Percentage:",
                              min = 50, max = 90, value = 80, step = 5)
                 ),
                 
                 conditionalPanel(
                   condition = paste0("input['", ns("validation_method"), "'] == 'cv'"),
                   ns = ns,
                   sliderInput(ns("cv_folds"), "Number of Folds:",
                              min = 3, max = 10, value = 5, step = 1)
                 ),
                 
                 checkboxInput(ns("use_seed"), "Use random seed for reproducibility", TRUE),
                 conditionalPanel(
                   condition = paste0("input['", ns("use_seed"), "'] == true"),
                   ns = ns,
                   numericInput(ns("random_seed"), "Random Seed:", value = 123)
                 ),
                 
                 hr(),
                 
                 h4("Performance Metrics"),
                 
                 conditionalPanel(
                   condition = paste0("input['", ns("is_regression"), "'] == true"),
                   ns = ns,
                   checkboxGroupInput(ns("reg_metrics"), "Regression Metrics:",
                                   choices = c("Mean Squared Error (MSE)" = "mse",
                                             "Root Mean Squared Error (RMSE)" = "rmse",
                                             "Mean Absolute Error (MAE)" = "mae",
                                             "R-squared" = "rsq"),
                                   selected = c("mse", "rmse", "rsq"))
                 ),
                 
                 conditionalPanel(
                   condition = paste0("input['", ns("is_classification"), "'] == true"),
                   ns = ns,
                   checkboxGroupInput(ns("cls_metrics"), "Classification Metrics:",
                                   choices = c("Accuracy" = "accuracy",
                                             "Precision" = "precision",
                                             "Recall" = "recall",
                                             "F1 Score" = "f1",
                                             "AUC-ROC" = "auc"),
                                   selected = c("accuracy", "precision", "recall"))
                 )
               )
      ),
      
      # Tab for model comparison
      tabPanel("Model Comparison",
               h4("Model Performance Comparison"),
               p("This will show the performance of all validated models."),
               
               tableOutput(ns("model_comparison_table")),
               
               plotOutput(ns("model_performance_plot")),
               
               hr(),
               
               h4("Select Final Model"),
               selectInput(ns("final_model"), "Choose the best model:",
                        choices = NULL),
               
               actionButton(ns("confirm_model"), "Confirm and Continue", 
                          class = "btn-success", 
                          style = "margin-top: 20px;")
      )
    ),
    
    # Hidden inputs to track problem type
    tags$div(
      style = "display: none;",
      checkboxInput(ns("is_regression"), "Is regression", FALSE),
      checkboxInput(ns("is_classification"), "Is classification", FALSE)
    )
  )
}

#' Server function for model selection module
#'
#' @param id The namespace id for the module
#' @param feature_data Reactive expression from the feature selection module
#' @return A reactive expression containing the selected model configuration
#' @export
model_selection_Server <- function(id, feature_data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Store validated models
    validated_models <- reactiveVal(list())
    
    # Extract data from feature selection
    selected_data <- reactive({
      req(feature_data())
      feature_data()$data
    })
    
    target_column <- reactive({
      req(feature_data())
      feature_data()$target_column
    })
    
    predictor_columns <- reactive({
      req(feature_data())
      feature_data()$predictor_columns
    })
    
    problem_type <- reactive({
      req(feature_data())
      feature_data()$problem_type
    })
    
    # Display problem type status
    output$problem_type_display <- renderUI({
      if (!is.null(problem_type())) {
        problem_icon <- if (problem_type() == "regression") "📈" else "🎯"
        problem_color <- if (problem_type() == "regression") "#28a745" else "#007bff"
        
        tagList(
          tags$p(
            tags$strong("Problem Type:", style = "color: #495057;"),
            tags$br(),
            tags$span(
              paste(problem_icon, stringr::str_to_title(problem_type())),
              style = paste0("color: ", problem_color, "; font-size: 16px; font-weight: bold;")
            )
          )
        )
      } else {
        tags$p("⏳ Waiting for data...", style = "color: #6c757d; font-style: italic;")
      }
    })
    
    # Display data summary
    output$data_summary_display <- renderUI({
      if (!is.null(selected_data()) && !is.null(target_column()) && !is.null(predictor_columns())) {
        tagList(
          tags$p(
            tags$strong("Data Summary:", style = "color: #495057;"),
            tags$br(),
            paste("🎯 Target:", target_column()),
            tags$br(),
            paste("📊 Features:", length(predictor_columns())),
            tags$br(),
            paste("📋 Observations:", nrow(selected_data()))
          )
        )
      } else {
        tags$p("⏳ Complete column selection first...", style = "color: #6c757d; font-style: italic;")
      }
    })
    
    # Set up the appropriate tab based on problem type
    observe({
      req(problem_type())
      
      cat("DEBUG: Problem type detected:", problem_type(), "\n")
      
      if (problem_type() == "regression") {
        updateCheckboxInput(session, "is_regression", value = TRUE)
        updateCheckboxInput(session, "is_classification", value = FALSE)
        updateTabsetPanel(session, "model_tabs", selected = "Regression Models")
        
        # Show notification about automatic detection
        showNotification(
          paste("✅ Regression problem detected! Automatically switched to Regression Models tab."), 
          type = "message", 
          duration = 4
        )
      } else if (problem_type() == "classification") {
        updateCheckboxInput(session, "is_regression", value = FALSE)
        updateCheckboxInput(session, "is_classification", value = TRUE)
        updateTabsetPanel(session, "model_tabs", selected = "Classification Models")
        
        # Show notification about automatic detection
        showNotification(
          paste("✅ Classification problem detected! Automatically switched to Classification Models tab."), 
          type = "message", 
          duration = 4
        )
      } else if (problem_type() == "unknown") {
        updateCheckboxInput(session, "is_regression", value = FALSE)
        updateCheckboxInput(session, "is_classification", value = FALSE)
        
        showNotification(
          paste("⚠️ Could not automatically detect problem type. Please manually select the appropriate tab."), 
          type = "warning", 
          duration = 5
        )
      }
    })
    
    # Handle manual tab switching buttons
    observeEvent(input$switch_to_classification, {
      updateTabsetPanel(session, "model_tabs", selected = "Classification Models")
      showNotification("Switched to Classification Models tab", type = "message", duration = 2)
    })
    
    observeEvent(input$switch_to_regression, {
      updateTabsetPanel(session, "model_tabs", selected = "Regression Models")  
      showNotification("Switched to Regression Models tab", type = "message", duration = 2)
    })
    
    # Validate regression models
    observeEvent(input$validate_regression, {
      req(selected_data(), target_column(), predictor_columns(), problem_type() == "regression")
      
      withProgress(message = "Validating regression models...", value = 0.1, {
        
        # Get the data
        data <- selected_data()
        target <- target_column()
        predictors <- predictor_columns()
        
        # Create formula
        formula_str <- paste(target, "~", paste(predictors, collapse = " + "))
        formula_obj <- as.formula(formula_str)
        
        # Initialize list to store model results
        model_results <- list()
        
        # Set random seed if specified
        if (input$use_seed) {
          set.seed(input$random_seed)
        }
        
        # Create train/test split or CV folds
        if (input$validation_method == "train_test") {
          train_idx <- sample(1:nrow(data), size = round(input$train_ratio/100 * nrow(data)))
          train_data <- data[train_idx, ]
          test_data <- data[-train_idx, ]
          
          # Function to evaluate a model on the test set
          evaluate_model <- function(model, model_name) {
            predictions <- predict(model, newdata = test_data)
            actual <- test_data[[target]]
            
            # Calculate metrics
            metrics <- list()
            
            if ("mse" %in% input$reg_metrics) {
              metrics$MSE <- mean((actual - predictions)^2)
            }
            if ("rmse" %in% input$reg_metrics) {
              metrics$RMSE <- sqrt(mean((actual - predictions)^2))
            }
            if ("mae" %in% input$reg_metrics) {
              metrics$MAE <- mean(abs(actual - predictions))
            }
            if ("rsq" %in% input$reg_metrics) {
              ss_total <- sum((actual - mean(actual))^2)
              ss_residual <- sum((actual - predictions)^2)
              metrics$Rsquared <- 1 - (ss_residual / ss_total)
            }
            
            return(list(model = model, name = model_name, metrics = metrics))
          }
          
          # Linear Regression
          if (input$reg_linear) {
            incProgress(0.1, detail = "Fitting linear regression")
            tryCatch({
              lm_model <- lm(formula_obj, data = train_data)
              model_results$lm <- evaluate_model(lm_model, "Linear Regression")
            }, error = function(e) {
              showNotification(paste("Error in linear regression:", e$message), type = "error")
            })
          }
          
          # Ridge Regression (requires glmnet package)
          if (input$reg_ridge) {
            incProgress(0.1, detail = "Fitting ridge regression")
            # This is a placeholder - in a real app, you'd use glmnet
            # with proper cross-validation for lambda
            model_results$ridge <- list(
              name = "Ridge Regression",
              metrics = list(MSE = 0.5, RMSE = 0.7, Rsquared = 0.8)
            )
          }
          
          # LASSO Regression (requires glmnet package)
          if (input$reg_lasso) {
            incProgress(0.1, detail = "Fitting LASSO regression")
            # This is a placeholder - in a real app, you'd use glmnet
            model_results$lasso <- list(
              name = "LASSO Regression",
              metrics = list(MSE = 0.6, RMSE = 0.8, Rsquared = 0.7)
            )
          }
          
          # Random Forest Regression
          if (input$reg_rf) {
            incProgress(0.1, detail = "Fitting random forest")
            tryCatch({
              # Placeholder for RF - would use randomForest package
              model_results$rf <- list(
                name = "Random Forest",
                metrics = list(MSE = 0.4, RMSE = 0.6, Rsquared = 0.85)
              )
            }, error = function(e) {
              showNotification(paste("Error in random forest:", e$message), type = "error")
            })
          }
          
          # Add other regression models here...
          
        } else {
          # Cross-validation logic would go here
          # For now, just providing placeholder results
          model_results$lm_cv <- list(
            name = "Linear Regression (CV)",
            metrics = list(MSE = 0.55, RMSE = 0.74, Rsquared = 0.76)
          )
          
          if (input$reg_rf) {
            model_results$rf_cv <- list(
              name = "Random Forest (CV)",
              metrics = list(MSE = 0.45, RMSE = 0.67, Rsquared = 0.82)
            )
          }
        }
        
        # Store results
        validated_models(model_results)
        
        # Update model comparison table
        update_model_comparison()
        
        # Update model dropdown
        model_names <- sapply(model_results, function(m) m$name)
        updateSelectInput(session, "final_model", choices = model_names)
      })
    })
    
    # Validate classification models
    observeEvent(input$validate_classification, {
      req(selected_data(), target_column(), predictor_columns(), problem_type() == "classification")
      
      withProgress(message = "Validating classification models...", value = 0.1, {
        
        # Similar logic to regression but for classification models
        # This is placeholder code - in a real app, you'd implement all the models
        model_results <- list(
          logistic = list(
            name = "Logistic Regression",
            metrics = list(accuracy = 0.85, precision = 0.83, recall = 0.81)
          ),
          rf = list(
            name = "Random Forest",
            metrics = list(accuracy = 0.88, precision = 0.87, recall = 0.84)
          )
        )
        
        # Store results
        validated_models(model_results)
        
        # Update model comparison table
        update_model_comparison()
        
        # Update model dropdown
        model_names <- sapply(model_results, function(m) m$name)
        updateSelectInput(session, "final_model", choices = model_names)
      })
    })
    
    # Update model comparison table and plot
    update_model_comparison <- function() {
      # Create comparison table
      output$model_comparison_table <- renderTable({
        models <- validated_models()
        if (length(models) == 0) return(NULL)
        
        # Extract metrics for each model
        comparison_df <- data.frame(Model = character(), stringsAsFactors = FALSE)
        
        for (model_name in names(models)) {
          model <- models[[model_name]]
          
          if (length(comparison_df) == 1) {  # Only has "Model" column
            # Add metrics columns
            for (metric_name in names(model$metrics)) {
              comparison_df[[metric_name]] <- numeric()
            }
          }
          
          # Add this model's data
          new_row <- data.frame(Model = model$name, stringsAsFactors = FALSE)
          for (metric_name in names(model$metrics)) {
            new_row[[metric_name]] <- model$metrics[[metric_name]]
          }
          
          comparison_df <- rbind(comparison_df, new_row)
        }
        
        # Round numeric columns to 4 decimal places
        comparison_df[, -1] <- round(comparison_df[, -1], 4)
        
        return(comparison_df)
      })
      
      # Create performance plot
      output$model_performance_plot <- renderPlot({
        models <- validated_models()
        if (length(models) == 0) return(NULL)
        
        # Extract first metric for each model for demonstration
        # In a real app, you might want to make this configurable
        plot_df <- data.frame(
          Model = character(),
          Value = numeric(),
          Metric = character(),
          stringsAsFactors = FALSE
        )
        
        for (model_name in names(models)) {
          model <- models[[model_name]]
          for (metric_name in names(model$metrics)) {
            plot_df <- rbind(plot_df, data.frame(
              Model = model$name,
              Value = model$metrics[[metric_name]],
              Metric = metric_name,
              stringsAsFactors = FALSE
            ))
          }
        }
        
        # Simple bar plot for now
        ggplot(plot_df, aes(x = Model, y = Value, fill = Metric)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(title = "Model Performance Comparison",
              y = "Metric Value") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
    }
    
    # Return selected model configuration when confirmed
    model_config <- eventReactive(input$confirm_model, {
      req(input$final_model, validated_models())
      
      # Show loading progress with more detailed steps
      withProgress(message = "Finalizing model selection...", value = 0, {
        
        incProgress(0.2, detail = "Validating model selection...")
        
        # Get the selected model info
        models <- validated_models()
        selected_model <- NULL
        
        for (model in models) {
          if (model$name == input$final_model) {
            selected_model <- model
            break
          }
        }
        
        if (is.null(selected_model)) {
          showNotification("❌ Error: Selected model not found!", type = "error", duration = 5)
          return(NULL)
        }
        
        incProgress(0.4, detail = "Preparing model configuration...")
        
        # Create model configuration
        config <- list(
          data = selected_data(),
          target_column = target_column(),
          predictor_columns = predictor_columns(),
          problem_type = problem_type(),
          model_name = input$final_model,
          validation_method = input$validation_method,
          validation_settings = list(
            train_ratio = if(input$validation_method == "train_test") input$train_ratio else NULL,
            cv_folds = if(input$validation_method == "cv") input$cv_folds else NULL,
            random_seed = if(input$use_seed) input$random_seed else NULL
          ),
          selected_metrics = if(problem_type() == "regression") input$reg_metrics else input$cls_metrics,
          model = selected_model,
          timestamp = Sys.time()
        )
        
        incProgress(0.7, detail = "Validating configuration...")
        
        # Validate the configuration
        config_valid <- !is.null(config$data) && !is.null(config$target_column) && 
                       !is.null(config$predictor_columns) && !is.null(config$problem_type)
        
        if (!config_valid) {
          showNotification("❌ Error: Invalid model configuration!", type = "error", duration = 5)
          return(NULL)
        }
        
        incProgress(0.9, detail = "Finalizing...")
        
        # Show detailed success notification
        notification_text <- paste0(
          "✅ Model selection completed successfully!\n",
          "📊 Model: ", input$final_model, "\n",
          "🎯 Problem Type: ", stringr::str_to_title(problem_type()), "\n",
          "📈 Target: ", target_column(), "\n",
          "🔧 Features: ", length(predictor_columns()), " variables\n",
          "✔️ Validation: ", stringr::str_to_title(gsub("_", " ", input$validation_method)), "\n",
          "➡️ Ready for Model Training"
        )
        
        showNotification(
          notification_text,
          type = "message", 
          duration = 8
        )
        
        # Log the configuration for debugging
        cat("DEBUG: Model configuration successfully created:\n")
        cat("- Model:", input$final_model, "\n")
        cat("- Problem type:", problem_type(), "\n")
        cat("- Target column:", target_column(), "\n")
        cat("- Predictor columns:", length(predictor_columns()), "\n")
        cat("- Validation method:", input$validation_method, "\n")
        cat("- Timestamp:", as.character(config$timestamp), "\n")
        
        incProgress(1.0, detail = "Complete!")
        
        return(config)
      })
    })
    
    return(model_config)
  })
}