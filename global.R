# global.R

# Function to check and install required packages
check_and_install_packages <- function() {
  # Required packages for the ML Model Builder
  required_packages <- c(
    "shiny", "shinydashboard", "shinyjs", "dplyr", "tidyverse", 
    "ggplot2", "plotly", "DT", "caret", "randomForest", "e1071", 
    "glmnet", "rpart", "rpart.plot", "corrplot", "car", "tidyr", "readr", "stringr",
    "rmarkdown", "knitr", "lubridate", "scales", "viridis"
  )
  
  # Check which packages are missing
  missing_packages <- setdiff(required_packages, rownames(installed.packages()))
  
  if (length(missing_packages) > 0) {
    cat("Missing packages detected:", paste(missing_packages, collapse = ", "), "\n")
    cat("Attempting to install missing packages...\n")
    
    # Try to install pak if not available
    if (!"pak" %in% rownames(installed.packages())) {
      cat("Installing pak package for faster installation...\n")
      tryCatch({
        install.packages("pak", repos = "https://r-lib.github.io/p/pak/stable/")
      }, error = function(e) {
        cat("Note: pak installation failed, using standard install.packages\n")
      })
    }
    
    # Install missing packages
    for (pkg in missing_packages) {
      tryCatch({
        if ("pak" %in% rownames(installed.packages())) {
          pak::pak(pkg)
        } else {
          install.packages(pkg, dependencies = TRUE)
        }
        cat("✅", pkg, "installed successfully\n")
      }, error = function(e) {
        cat("❌ Failed to install", pkg, ":", e$message, "\n")
        cat("Please install", pkg, "manually using: install.packages('", pkg, "')\n")
      })
    }
  }
  
  # Load packages with error handling
  failed_loads <- c()
  for (pkg in required_packages) {
    tryCatch({
      library(pkg, character.only = TRUE, quietly = TRUE)
    }, error = function(e) {
      failed_loads <<- c(failed_loads, pkg)
      cat("❌ Failed to load", pkg, ":", e$message, "\n")
    })
  }
  
  if (length(failed_loads) > 0) {
    stop("Cannot start application. Missing packages: ", paste(failed_loads, collapse = ", "))
  }
  
  cat("✅ All packages loaded successfully!\n")
}

# Check and install packages
check_and_install_packages()

# Load necessary libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(caret)
library(randomForest)
library(e1071)
library(DT) # For interactive data tables
library(plotly) # For interactive plots
library(corrplot) # For correlation plots
library(car) # For statistical tests
library(shinyjs) # For JavaScript functionality

# Define global variables
data <- NULL
cleaned_data <- NULL
selected_columns <- NULL
model <- NULL
model_type <- NULL

# Function to load data
load_data <- function(file) {
  data <<- read.csv(file, stringsAsFactors = FALSE)
}

# Function to clean data
clean_data <- function(data) {
  # Implement data cleaning steps here
  cleaned_data <<- data %>%
    mutate_if(is.character, as.factor) %>%
    drop_na()
}

# Function to get column names
get_column_names <- function() {
  return(colnames(cleaned_data))
}