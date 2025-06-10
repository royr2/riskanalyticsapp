library(testthat)
library(dplyr)
library(tidyr)

# Load the model training module
source("../R/model/model_training_module.R")

# Sample data for testing
test_data <- data.frame(
  predictor1 = rnorm(100),
  predictor2 = rnorm(100),
  target = rnorm(100)
)

# Test model training function
test_that("Model training works correctly", {
  model <- train_model(test_data, target = "target", predictors = c("predictor1", "predictor2"))
  
  expect_s3_class(model, "lm")  # Assuming the model is a linear model
  expect_equal(length(coef(model)), 3)  # Intercept + 2 predictors
})

# Test model diagnostics function
test_that("Model diagnostics return correct metrics", {
  model <- train_model(test_data, target = "target", predictors = c("predictor1", "predictor2"))
  diagnostics <- get_model_diagnostics(model)
  
  expect_true("r_squared" %in% names(diagnostics))
  expect_true("p_value" %in% names(diagnostics))
  expect_true(diagnostics$r_squared >= 0)  # R-squared should be non-negative
})