library(shiny)
library(shinydashboard)

# Source all modules
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

ui <- dashboardPage(
  dashboardHeader(title = "ML Analytics Suite"),
  
  dashboardSidebar(
    sidebarMenu(
      # Main application sections
      menuItem("🏠 Dashboard", tabName = "dashboard_home", icon = icon("tachometer-alt")),
      
      # ML Model Building workflow
      menuItem("🤖 ML Model Builder", icon = icon("cogs"),
        menuSubItem("Data Import", tabName = "data_import", icon = icon("upload")),
        menuSubItem("Data Preprocessing", tabName = "data_preprocessing", icon = icon("broom")),
        menuSubItem("Column Selection", tabName = "column_selection", icon = icon("columns")),
        menuSubItem("Feature Selection", tabName = "feature_selection", icon = icon("filter")),
        menuSubItem("Model Selection", tabName = "model_selection", icon = icon("brain")),
        menuSubItem("Model Training", tabName = "model_training", icon = icon("play")),
        menuSubItem("Model Diagnostics", tabName = "model_diagnostics", icon = icon("chart-line"))
      ),
      
      # Dashboard Analytics
      menuItem("📊 Analytics Dashboard", icon = icon("chart-bar"),
        menuSubItem("Data Overview", tabName = "overview_dashboard", icon = icon("eye")),
        menuSubItem("Interactive Charts", tabName = "charts_dashboard", icon = icon("chart-area")),
        menuSubItem("KPI Dashboard", tabName = "kpi_dashboard", icon = icon("bullseye")),
        menuSubItem("Reports", tabName = "reports_dashboard", icon = icon("file-alt"))
      ),
      
      # Settings and help
      menuItem("⚙️ Settings", tabName = "settings", icon = icon("cog")),
      menuItem("❓ Help", tabName = "help", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js")
    ),
    
    tabItems(
      # Dashboard Home
      tabItem(tabName = "dashboard_home",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Welcome to ML Analytics Suite",
                    
                    h3("🎯 What would you like to do today?"),
                    
                    fluidRow(
                      column(6,
                             wellPanel(
                               style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white;",
                               h4("🤖 Build ML Models", style = "color: white;"),
                               p("Complete machine learning workflow from data import to model diagnostics.", style = "color: white;"),
                               actionButton("goto_ml", "Start ML Workflow", 
                                          class = "btn-light", 
                                          onclick = "Shiny.setInputValue('sidebar_menu', 'data_import', {priority: 'event'})")
                             )
                      ),
                      column(6,
                             wellPanel(
                               style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); color: white;",
                               h4("📊 Explore Data", style = "color: white;"),
                               p("Interactive dashboards, charts, KPIs, and automated reporting.", style = "color: white;"),
                               actionButton("goto_dashboard", "Start Analytics", 
                                          class = "btn-light",
                                          onclick = "Shiny.setInputValue('sidebar_menu', 'overview_dashboard', {priority: 'event'})")
                             )
                      )
                    ),
                    
                    hr(),
                    
                    fluidRow(
                      valueBoxOutput("home_data_status", width = 3),
                      valueBoxOutput("home_models_status", width = 3),
                      valueBoxOutput("home_charts_status", width = 3),
                      valueBoxOutput("home_reports_status", width = 3)
                    ),
                    
                    hr(),
                    
                    h4("🚀 Quick Start Guide"),
                    tags$ol(
                      tags$li("Import your data using the Data Import module"),
                      tags$li("Explore your data with the Analytics Dashboard"),
                      tags$li("Build ML models with the ML Model Builder"),
                      tags$li("Generate reports and share insights")
                    )
                )
              )
      ),
      
      # ML Model Building Workflow
      tabItem(tabName = "data_import",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Data Import",
                    data_import_UI("data_import")
                )
              )
      ),
      
      tabItem(tabName = "data_preprocessing",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Data Preprocessing",
                    data_preprocessing_UI("data_preprocessing")
                )
              )
      ),
      
      tabItem(tabName = "column_selection",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Column Selection",
                    column_selection_UI("column_selection")
                )
              )
      ),
      
      tabItem(tabName = "feature_selection",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Feature Selection",
                    feature_selection_UI("feature_selection")
                )
              )
      ),
      
      tabItem(tabName = "model_selection",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Model Selection",
                    model_selection_UI("model_selection")
                )
              )
      ),
      
      tabItem(tabName = "model_training",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Model Training",
                    model_training_UI("model_training")
                )
              )
      ),
      
      tabItem(tabName = "model_diagnostics",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Model Diagnostics",
                    model_diagnostics_UI("model_diagnostics")
                )
              )
      ),
      
      # Analytics Dashboard
      tabItem(tabName = "overview_dashboard",
              fluidRow(
                box(width = 12, status = "success", solidHeader = TRUE,
                    title = "Data Overview Dashboard",
                    overview_dashboard_UI("overview_dashboard")
                )
              )
      ),
      
      tabItem(tabName = "charts_dashboard",
              fluidRow(
                box(width = 12, status = "success", solidHeader = TRUE,
                    title = "Interactive Charts Dashboard",
                    charts_dashboard_UI("charts_dashboard")
                )
              )
      ),
      
      tabItem(tabName = "kpi_dashboard",
              fluidRow(
                box(width = 12, status = "success", solidHeader = TRUE,
                    title = "KPI Dashboard",
                    kpi_dashboard_UI("kpi_dashboard")
                )
              )
      ),
      
      tabItem(tabName = "reports_dashboard",
              fluidRow(
                box(width = 12, status = "success", solidHeader = TRUE,
                    title = "Reports Dashboard",
                    reports_dashboard_UI("reports_dashboard")
                )
              )
      ),
      
      # Settings and Help
      tabItem(tabName = "settings",
              fluidRow(
                box(width = 12, status = "warning", solidHeader = TRUE,
                    title = "Application Settings",
                    h4("Settings functionality coming soon..."),
                    p("This section will include:")
                    tags$ul(
                      tags$li("User preferences"),
                      tags$li("Data source configurations"),
                      tags$li("Export settings"),
                      tags$li("Notification preferences")
                    )
                )
              )
      ),
      
      tabItem(tabName = "help",
              fluidRow(
                box(width = 12, status = "info", solidHeader = TRUE,
                    title = "Help & Documentation",
                    
                    h4("📖 User Guide"),
                    p("Welcome to the ML Analytics Suite! This application combines machine learning model building with comprehensive data analytics dashboards."),
                    
                    h4("🤖 ML Model Builder"),
                    p("Follow the workflow from left to right:"),
                    tags$ol(
                      tags$li("Data Import: Upload CSV files or load sample datasets"),
                      tags$li("Data Preprocessing: Clean and prepare your data"),
                      tags$li("Column Selection: Choose target and predictor variables"),
                      tags$li("Feature Selection: Select the most relevant features"),
                      tags$li("Model Selection: Choose and configure ML algorithms"),
                      tags$li("Model Training: Train your selected models"),
                      tags$li("Model Diagnostics: Evaluate model performance")
                    ),
                    
                    h4("📊 Analytics Dashboard"),
                    tags$ul(
                      tags$li("Data Overview: Get comprehensive insights about your data"),
                      tags$li("Interactive Charts: Create custom visualizations"),
                      tags$li("KPI Dashboard: Track key performance indicators"),
                      tags$li("Reports: Generate automated reports")
                    ),
                    
                    h4("💡 Tips"),
                    tags$ul(
                      tags$li("Start with the Data Import module to load your data"),
                      tags$li("Use the Analytics Dashboard to explore your data before modeling"),
                      tags$li("The application automatically detects problem types (regression/classification)"),
                      tags$li("All modules are connected - data flows automatically between steps")
                    )
                )
              )
      )
    )
  )
)