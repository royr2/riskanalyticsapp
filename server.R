library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

# Source ML modules
source("R/data/data_import_module.R")
source("R/data/data_preprocessing_module.R")
source("R/feature/column_selection_module.R")
source("R/feature/feature_selection_module.R")
source("R/model/model_selection_module.R")
source("R/model/model_training_module.R")
source("R/diagnostics/model_diagnostics_module.R")

# Source dashboard modules
source("R/dashboard/overview_dashboard_module.R")
source("R/dashboard/charts_dashboard_module.R")
source("R/dashboard/kpi_dashboard_module.R")
source("R/dashboard/reports_dashboard_module.R")

# Define server logic
server <- function(input, output, session) {
  
  # ML MODEL BUILDING WORKFLOW
  # Step 1: Data Import
  imported_data <- data_import_Server("data_import")
  
  # Step 2: Data Preprocessing
  preprocessed_data <- data_preprocessing_Server("data_preprocessing", imported_data)
  
  # Step 3: Column Selection
  column_config <- column_selection_Server("column_selection", preprocessed_data)
  
  # Step 4: Feature Selection
  feature_config <- feature_selection_Server("feature_selection", column_config)
  
  # Step 5: Model Selection
  model_config <- model_selection_Server("model_selection", feature_config)
  
  # Step 6: Model Training
  trained_model <- model_training_Server("model_training", model_config)
  
  # Step 7: Model Diagnostics
  diagnostic_results <- model_diagnostics_Server("model_diagnostics", trained_model)
  
  # ANALYTICS DASHBOARD MODULES
  # Use the most recent available data for dashboards
  dashboard_data <- reactive({
    # Priority order: feature_config > column_config > preprocessed_data > imported_data
    if (!is.null(feature_config()) && !is.null(feature_config()$data)) {
      feature_config()$data
    } else if (!is.null(column_config()) && !is.null(column_config()$data)) {
      column_config()$data
    } else if (!is.null(preprocessed_data())) {
      preprocessed_data()
    } else if (!is.null(imported_data())) {
      imported_data()
    } else {
      NULL
    }
  })
  
  # Dashboard modules
  overview_dashboard_Server("overview_dashboard", dashboard_data)
  charts_dashboard_Server("charts_dashboard", dashboard_data)
  kpi_dashboard_Server("kpi_dashboard", dashboard_data)
  reports_dashboard_Server("reports_dashboard", dashboard_data)
  
  # HOME DASHBOARD VALUE BOXES
  output$home_data_status <- renderValueBox({
    has_data <- !is.null(dashboard_data())
    
    valueBox(
      value = if (has_data) "✓ Ready" else "No Data",
      subtitle = "Data Status",
      icon = icon(if (has_data) "check-circle" else "exclamation-triangle"),
      color = if (has_data) "green" else "red"
    )
  })
  
  output$home_models_status <- renderValueBox({
    has_models <- !is.null(trained_model()) && !is.null(trained_model()$trained_models)
    
    valueBox(
      value = if (has_models) "✓ Trained" else "Not Ready",
      subtitle = "ML Models",
      icon = icon(if (has_models) "brain" else "clock"),
      color = if (has_models) "blue" else "yellow"
    )
  })
  
  output$home_charts_status <- renderValueBox({
    has_data <- !is.null(dashboard_data())
    
    valueBox(
      value = if (has_data) "✓ Available" else "No Data",
      subtitle = "Interactive Charts",
      icon = icon("chart-bar"),
      color = if (has_data) "purple" else "light-blue"
    )
  })
  
  output$home_reports_status <- renderValueBox({
    has_data <- !is.null(dashboard_data())
    
    valueBox(
      value = if (has_data) "✓ Ready" else "No Data",
      subtitle = "Reports",
      icon = icon("file-alt"),
      color = if (has_data) "orange" else "gray"
    )
  })
  
}