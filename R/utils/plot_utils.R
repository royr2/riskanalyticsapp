# File: /ml-model-builder/ml-model-builder/R/utils/plot_utils.R

# This file provides functions for generating plots and visualizations related to the model diagnostics.

generate_model_diagnostics_plot <- function(model, data) {
  # Function to generate diagnostic plots for the fitted model
  par(mfrow = c(2, 2))
  
  # Residuals vs Fitted
  plot(model$fitted.values, residuals(model), 
       xlab = "Fitted values", ylab = "Residuals", 
       main = "Residuals vs Fitted")
  abline(h = 0, col = "red")
  
  # Normal Q-Q
  qqnorm(residuals(model), main = "Normal Q-Q")
  qqline(residuals(model), col = "red")
  
  # Scale-Location
  plot(model$fitted.values, sqrt(abs(residuals(model))), 
       xlab = "Fitted values", ylab = "Sqrt |Residuals|", 
       main = "Scale-Location")
  
  # Residuals vs Leverage
  plot(model, which = 5)
}

plot_feature_importance <- function(model, feature_names) {
  # Function to plot feature importance for tree-based models
  if ("randomForest" %in% class(model)) {
    importance_values <- randomForest::importance(model)
    importance_df <- data.frame(Feature = feature_names, Importance = importance_values)
    importance_df <- importance_df[order(-importance_df$Importance), ]
    
    barplot(importance_df$Importance, names.arg = importance_df$Feature, 
            las = 2, main = "Feature Importance", col = "blue")
  } else {
    stop("Feature importance plotting is only available for random forest models.")
  }
}