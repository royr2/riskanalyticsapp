# Contents of /ml-model-builder/ml-model-builder/R/utils/model_utils.R

# Utility functions for model training and evaluation

# Function to train a model based on the selected algorithm
train_model <- function(model_type, formula, data) {
  if (model_type == "linear_regression") {
    model <- lm(formula, data = data)
  } else if (model_type == "logistic_regression") {
    model <- glm(formula, data = data, family = binomial)
  } else if (model_type == "random_forest") {
    library(randomForest)
    model <- randomForest(formula, data = data)
  } else if (model_type == "decision_tree") {
    library(rpart)
    model <- rpart(formula, data = data)
  } else {
    stop("Unsupported model type")
  }
  return(model)
}

# Function to evaluate model performance
evaluate_model <- function(model, data, target_column) {
  predictions <- predict(model, newdata = data)
  
  if (is.numeric(data[[target_column]])) {
    # Regression evaluation
    mse <- mean((data[[target_column]] - predictions)^2)
    r_squared <- summary(model)$r.squared
    return(list(MSE = mse, R_squared = r_squared))
  } else {
    # Classification evaluation
    confusion_matrix <- table(data[[target_column]], predictions > 0.5)
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    return(list(Confusion_Matrix = confusion_matrix, Accuracy = accuracy))
  }
}