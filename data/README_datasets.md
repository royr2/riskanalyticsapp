# Sample Datasets for ML Model Builder

This document describes the sample datasets included with the ML Model Builder application. Each dataset is designed to test different aspects of the machine learning workflow and represents common real-world scenarios.

## 1. Credit Risk Data (`credit_risk_data.csv`)
**Problem Type**: Binary Classification  
**Target Variable**: `Default` (0 = No Default, 1 = Default)  
**Use Case**: Predicting loan default risk  

### Features:
- `CustomerID`: Unique identifier
- `Age`: Customer age (24-52)
- `Income`: Annual income ($28,000-$95,000)
- `CreditScore`: Credit score (520-780)
- `LoanAmount`: Requested loan amount ($8,000-$32,000)
- `EmploymentYears`: Years of employment (0-22)
- `HasMortgage`: Whether customer has a mortgage (0/1)
- `MaritalStatus`: Single, Married, Divorced
- `Education`: HighSchool, Bachelor, Master, PhD
- `Region`: North, South, East, West

**Analysis Opportunities**:
- Feature importance analysis
- Correlation between income and default rate
- Impact of education level on creditworthiness
- Regional differences in default patterns

---

## 2. House Prices Data (`house_prices_data.csv`)
**Problem Type**: Regression  
**Target Variable**: `Price` (House price in dollars)  
**Use Case**: Real estate price prediction  

### Features:
- `HouseID`: Unique identifier
- `Size_SqFt`: House size in square feet (1,350-3,450)
- `Bedrooms`: Number of bedrooms (1-5)
- `Bathrooms`: Number of bathrooms (1-4)
- `Age_Years`: Age of the house (2-40 years)
- `GarageSpaces`: Number of garage spaces (0-3)
- `HasPool`: Whether house has a pool (0/1)
- `HasFireplace`: Whether house has a fireplace (0/1)
- `Neighborhood`: Urban, Suburban, Rural
- `School_Rating`: Local school rating (3-9)
- `Crime_Rate`: Local crime rate (0.9-6.8)
- `Distance_Downtown`: Distance to downtown (1.2-28.2 miles)
- `Property_Tax`: Annual property tax ($2,600-$5,400)

**Analysis Opportunities**:
- Size vs. price relationship
- Neighborhood impact on pricing
- Feature engineering (price per square foot)
- Non-linear relationships exploration

---

## 3. Product Sales Data (`product_sales_data.csv`)
**Problem Type**: Regression  
**Target Variable**: `Sales` (Number of units sold)  
**Use Case**: Sales forecasting and marketing optimization  

### Features:
- `ProductID`: Unique identifier
- `Category`: Electronics, Clothing, Home, Sports, Books
- `Price`: Product price ($14.99-$799.99)
- `Rating`: Customer rating (3.7-4.8)
- `Reviews`: Number of reviews (67-456)
- `Brand`: Various brand names
- `InStock`: Whether product is in stock (0/1)
- `Season`: Winter, Spring, Summer, Fall
- `Discount`: Discount percentage (0-0.2)
- `Marketing_Spend`: Marketing budget ($800-$10,000)

**Analysis Opportunities**:
- Price elasticity analysis
- Seasonal sales patterns
- Marketing ROI analysis
- Category performance comparison

---

## 4. Employee Promotion Data (`employee_promotion_data.csv`)
**Problem Type**: Binary Classification  
**Target Variable**: `Promotion` (0 = No Promotion, 1 = Promotion)  
**Use Case**: HR analytics and promotion prediction  

### Features:
- `EmployeeID`: Unique identifier
- `Age`: Employee age (24-45)
- `Department`: Sales, Engineering, Marketing, HR, Finance, IT
- `Experience_Years`: Years of experience (1-18)
- `Salary`: Annual salary ($38,000-$88,000)
- `PerformanceScore`: Performance rating (2.6-4.6)
- `TrainingHours`: Training hours completed (10-42)
- `Overtime_Hours`: Overtime hours worked (2-18)
- `JobSatisfaction`: Job satisfaction score (4-9)
- `Education`: HighSchool, Bachelor, Master, PhD

**Analysis Opportunities**:
- Performance vs. promotion relationship
- Department-wise promotion patterns
- Impact of training on career advancement
- Salary equity analysis

---

## 5. Student Performance Data (`student_performance_data.csv`)
**Problem Type**: Multi-class Classification  
**Target Variable**: `Final_Grade` (A, B, C, D)  
**Use Case**: Educational analytics and grade prediction  

### Features:
- `StudentID`: Unique identifier
- `Math_Score`: Math test score (61-95)
- `Reading_Score`: Reading test score (65-93)
- `Writing_Score`: Writing test score (63-97)
- `Study_Hours`: Weekly study hours (4-30)
- `Attendance_Rate`: Class attendance rate (0.80-0.99)
- `Parental_Education`: HighSchool, Bachelor, Master, PhD
- `Family_Income`: Family income ($28,000-$105,000)
- `Tutoring`: Whether student receives tutoring (0/1)
- `Extracurricular`: Number of extracurricular activities (0-5)
- `Grade_Level`: Grade level (9-12)

**Analysis Opportunities**:
- Academic performance predictors
- Socioeconomic impact on education
- Tutoring effectiveness analysis
- Multi-class classification challenges

---

## 6. Boston Housing Sample (`boston_housing_sample.csv`)
**Problem Type**: Regression  
**Target Variable**: `MEDV` (Median home value in $1000s)  
**Use Case**: Classic regression benchmark dataset  

### Features:
- `CRIM`: Crime rate per capita
- `ZN`: Proportion of residential land zoned for lots over 25,000 sq.ft
- `INDUS`: Proportion of non-retail business acres
- `CHAS`: Charles River dummy variable (1 if tract bounds river)
- `NOX`: Nitric oxides concentration
- `RM`: Average number of rooms per dwelling
- `AGE`: Proportion of owner-occupied units built prior to 1940
- `DIS`: Weighted distances to employment centers
- `RAD`: Index of accessibility to radial highways
- `TAX`: Property tax rate per $10,000
- `PTRATIO`: Pupil-teacher ratio by town
- `B`: Proportion of blacks by town
- `LSTAT`: % lower status of the population

**Analysis Opportunities**:
- Classic regression analysis
- Feature correlation exploration
- Non-linear relationship modeling
- Benchmark comparison

---

## Testing Recommendations

### For Data Import Testing:
- Use different datasets to test CSV parsing
- Test with various column types and missing values

### For Preprocessing Testing:
- **Missing Values**: Introduce some missing values to test imputation
- **Data Types**: Test automatic type detection and conversion
- **Scaling**: Test normalization on different numeric ranges

### For Feature Selection Testing:
- **High Correlation**: Use house prices data (size vs. price)
- **Mixed Types**: Use employee data (categorical + numeric)
- **Many Features**: Use student performance data

### For Model Selection Testing:
- **Binary Classification**: Credit risk, employee promotion
- **Multi-class Classification**: Student grades
- **Regression**: House prices, product sales
- **Complex Relationships**: Boston housing (non-linear patterns)

### For Diagnostics Testing:
- **Good Fit**: House prices (strong size-price relationship)
- **Overfitting**: Student data with many features
- **Class Imbalance**: Credit risk data
- **Outliers**: Product sales data (wide price range)

---

## Usage Tips

1. **Start Simple**: Begin with house prices data for regression or credit risk for classification
2. **Test Edge Cases**: Use Boston housing for complex relationships
3. **Compare Models**: Use the same dataset across different algorithms
4. **Feature Engineering**: Product sales data is great for creating new features
5. **Business Context**: Each dataset has realistic business scenarios for interpretation

## Data Quality Notes

- All datasets are synthetic but follow realistic distributions
- No missing values in base datasets (add them manually to test imputation)
- Balanced representation across categories
- Appropriate correlation structures between variables
- Realistic value ranges for each domain
