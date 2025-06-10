library(testthat)
library(shiny)

# Test for the data import functionality
test_that("CSV file is imported correctly", {
  # Simulate file upload
  test_file <- system.file("data", "sample_data.csv", package = "ml-model-builder")
  
  # Use the data import module function to read the CSV
  imported_data <- read.csv(test_file)
  
  # Check if the data is a data frame
  expect_true(is.data.frame(imported_data))
  
  # Check if the expected columns are present
  expected_columns <- c("column1", "column2", "column3")  # Replace with actual column names
  expect_true(all(expected_columns %in% colnames(imported_data)))
})

test_that("Error is thrown for invalid CSV file", {
  # Simulate invalid file upload
  invalid_file <- "invalid_file.csv"
  
  # Expect an error when trying to read an invalid CSV
  expect_error(read.csv(invalid_file), "No such file or directory")
})