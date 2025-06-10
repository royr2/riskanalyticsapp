# Utility functions for data cleaning tasks

# Function to remove NA values from a data frame
remove_na <- function(data) {
  return(na.omit(data))
}

# Function to standardize column names
standardize_column_names <- function(data) {
  names(data) <- tolower(gsub(" ", "_", names(data)))
  return(data)
}

# Function to convert categorical variables to factors
convert_to_factors <- function(data) {
  categorical_cols <- sapply(data, is.character)
  data[categorical_cols] <- lapply(data[categorical_cols], as.factor)
  return(data)
}

# Function to handle outliers using IQR method
remove_outliers <- function(data) {
  numeric_cols <- sapply(data, is.numeric)
  for (col in names(data)[numeric_cols]) {
    Q1 <- quantile(data[[col]], 0.25)
    Q3 <- quantile(data[[col]], 0.75)
    IQR <- Q3 - Q1
    data <- data[data[[col]] >= (Q1 - 1.5 * IQR) & data[[col]] <= (Q3 + 1.5 * IQR), ]
  }
  return(data)
}