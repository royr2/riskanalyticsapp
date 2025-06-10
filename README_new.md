# ML Model Builder - Shiny Application

A comprehensive R Shiny application for building, validating, and documenting machine learning models with an intuitive step-by-step workflow.

## Features

### 🔄 Complete ML Workflow
- **Data Import**: Upload CSV files with configurable parsing options
- **Data Preprocessing**: Handle missing values, data types, normalization, and feature engineering
- **Column Selection**: Select target and predictor variables with automatic problem type detection
- **Feature Selection**: Apply various feature selection methods (filter, wrapper, embedded)
- **Model Selection**: Choose from multiple algorithms for regression and classification
- **Model Training**: Train models with hyperparameter tuning options
- **Model Diagnostics**: Comprehensive model validation and diagnostic tools

### 🎯 Key Capabilities
- **Automatic Problem Detection**: Determines if it's a regression or classification problem based on target variable
- **Interactive Visualizations**: Plotly-powered charts for better insights
- **Modular Architecture**: Well-structured codebase following R package development best practices
- **Responsive Design**: Modern dashboard UI with shinydashboard
- **Comprehensive Diagnostics**: Statistical tests, residual analysis, cross-validation, and more

## Installation

### Prerequisites
Make sure you have R (>= 4.0.0) installed on your system.

### Required Packages
Install the required packages by running:

```r
# Core packages
install.packages(c(
  "shiny", "shinydashboard", "DT", "plotly", "ggplot2",
  "dplyr", "tidyverse", "caret", "randomForest", "e1071",
  "glmnet", "rpart", "corrplot", "car", "shinyjs"
))
```

### Running the Application

1. **Clone or download** this repository
2. **Set working directory** to the ml-model-builder folder
3. **Run the application**:

```r
# Option 1: Run directly
shiny::runApp()

# Option 2: Run from specific file
source("app.R")
```

## Usage Guide

### Step 1: Data Import
1. Click on "Data Import" in the sidebar
2. Upload a CSV file using the file input
3. Configure parsing options (separator, quote character, header)
4. Preview your data and verify it loaded correctly

### Step 2: Data Preprocessing
1. Navigate to "Data Preprocessing"
2. Handle missing values (remove or impute)
3. Convert data types as needed
4. Apply normalization if required
5. Perform basic feature engineering

### Step 3: Column Selection
1. Go to "Column Selection"
2. Select your target variable (what you want to predict)
3. Choose predictor variables (features for prediction)
4. The app will automatically detect if it's a regression or classification problem

### Step 4: Feature Selection
1. Open "Feature Selection"
2. Choose from various feature selection methods:
   - **Filter methods**: Correlation, Chi-squared, ANOVA
   - **Wrapper methods**: Recursive Feature Elimination
   - **Embedded methods**: LASSO, Ridge regression
3. Review selected features and their importance scores

### Step 5: Model Selection
1. Navigate to "Model Selection"
2. Based on your problem type, choose from available models:
   - **Regression**: Linear, Ridge, LASSO, Random Forest, SVM
   - **Classification**: Logistic, Random Forest, SVM, Naive Bayes
3. Configure model-specific parameters

### Step 6: Model Training
1. Go to "Model Training"
2. Configure training options:
   - Train/test split ratio
   - Random seed for reproducibility
   - Hyperparameter tuning options
3. Train your model and review results
4. Examine performance metrics and diagnostic plots

### Step 7: Model Diagnostics
1. Open "Model Diagnostics"
2. Run comprehensive diagnostic tests:
   - Residual analysis
   - Normality tests
   - Outlier detection
   - Cross-validation
   - Bootstrap analysis
3. Generate and download diagnostic reports

## Sample Data

The application includes sample datasets in the `data/` folder:
- `boston_housing_sample.csv`: Housing price prediction dataset
- `sample_data.csv`: Simple example dataset

## Application Structure

```
ml-model-builder/
├── app.R                 # Main application entry point
├── ui.R                  # User interface definition
├── server.R              # Server logic
├── global.R              # Global variables and libraries
├── R/                    # Modular components
│   ├── data/            # Data handling modules
│   ├── feature/         # Feature selection modules
│   ├── model/           # Model selection and training
│   ├── diagnostics/     # Model diagnostics
│   └── utils/           # Utility functions
├── www/                 # Web assets (CSS, JS)
├── data/               # Sample datasets
└── tests/              # Unit tests
```

## Development

### Adding New Models
To add new machine learning models:

1. Update `model_selection_module.R` to include the new model option
2. Implement training logic in `model_training_module.R`
3. Add appropriate diagnostic tests in `model_diagnostics_module.R`

### Adding New Feature Selection Methods
1. Extend `feature_selection_module.R` with new methods
2. Implement the selection algorithm
3. Update the UI to include the new option

### Contributing
1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Submit a pull request

## Troubleshooting

### Common Issues

1. **Package Installation Errors**
   - Make sure you have the latest R version
   - Install packages one by one to identify problematic ones
   - Check CRAN mirror settings

2. **File Upload Issues**
   - Ensure CSV files are properly formatted
   - Check file size limits
   - Verify column names don't contain special characters

3. **Memory Issues with Large Datasets**
   - Consider data sampling for exploration
   - Use data.table for better memory management
   - Increase R memory limits if needed

### Getting Help

- Check the R console for error messages
- Verify all required packages are installed
- Ensure your data is in the correct format
- Review the sample datasets for format examples

## License

This project is licensed under the MIT License.

## Acknowledgments

- Built with R Shiny and shinydashboard
- Uses various R packages for machine learning and visualization
- Inspired by best practices in ML workflow management
