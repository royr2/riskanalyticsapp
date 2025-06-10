library(testthat)
library(dplyr)
source("../R/data/data_preprocessing_module.R")

test_that("Data preprocessing works correctly", {
  # Create a sample dataframe for testing
  sample_data <- data.frame(
    A = c(1, 2, NA, 4),
    B = c("x", "y", "z", "w"),
    C = c(10, 20, 30, 40)
  )
  
  # Test data cleaning function
  cleaned_data <- clean_data(sample_data)
  expect_equal(nrow(cleaned_data), 3)  # Expecting 3 rows after removing NA
  
  # Test data transformation function
  transformed_data <- transform_data(cleaned_data)
  expect_true("C" %in% colnames(transformed_data))  # Check if column C exists after transformation
  
  # Test if the data types are correct
  expect_type(transformed_data$A, "double")  # Check if column A is numeric
  expect_type(transformed_data$B, "character")  # Check if column B is character
})