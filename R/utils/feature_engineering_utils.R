# Feature Engineering Utility Functions

# This file includes utility functions for feature engineering processes.

# Function to create polynomial features
create_polynomial_features <- function(data, degree = 2) {
  poly_features <- as.data.frame(sapply(data, function(x) {
    if (is.numeric(x)) {
      return(poly(x, degree, raw = TRUE))
    } else {
      return(NULL)
    }
  }))
  colnames(poly_features) <- paste0(colnames(data), "_poly", 1:degree)
  return(poly_features)
}

# Function to encode categorical variables
encode_categorical_variables <- function(data) {
  categorical_vars <- sapply(data, is.factor)
  if (any(categorical_vars)) {
    data <- model.matrix(~ . - 1, data = data)
  }
  return(data)
}

# Function to scale features
scale_features <- function(data) {
  numeric_vars <- sapply(data, is.numeric)
  data[numeric_vars] <- scale(data[numeric_vars])
  return(data)
}

# Function to handle missing values
impute_missing_values <- function(data, method = "mean") {
  for (col in names(data)) {
    if (any(is.na(data[[col]]))) {
      if (method == "mean" && is.numeric(data[[col]])) {
        data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
      } else if (method == "median" && is.numeric(data[[col]])) {
        data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
      } else if (method == "mode" && is.factor(data[[col]])) {
        mode_value <- as.character(names(sort(table(data[[col]]), decreasing = TRUE)[1]))
        data[[col]][is.na(data[[col]])] <- mode_value
      }
    }
  }
  return(data)
}