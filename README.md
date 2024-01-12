# **LEAD CLASSIFICATION PROJECT**

## Overview

This project aims to develop a model that effectively identifies the most promising leads (hot leads) for X Education, an online course provider, to improve their lead conversion rate.

## Problem Statement

- X Education experiences a low lead conversion rate of around 30%.
- The goal is to build a model that assigns lead scores to prioritize high-potential leads for sales outreach.
- The target lead conversion rate is 80%.

### Data

- **Size:** 9074 rows, 31 columns
- **Attributes:** Lead Source, Total Time Spent on Website, Total Visits, Last Activity, etc.
- **Target Variable:** 'Converted' (1 for converted, 0 for not converted)

### Goal

1. **Exploratory Data Analysis (EDA)** 
2. **Build a logistic regression model for lead scoring (0-100)** 
3. **Build a Naïve Bayes Classifier** 

## Solution

### Data Cleaning

- **Columns Removed:** Unnecessary columns and those with dominant levels were eliminated.
- **Index Columns:** Prospect ID and Lead Number were treated as index columns.
- **Categorical Variables:** Levels like 'Select' were handled to reduce ambiguity.

### Model Building

1. **Initial Model:** Utilized all variables, leading to high VIF values, indicating multicollinearity.
2. **Variable Selection:** Applied Step-AIC to identify and include only essential variables.
3. **Final Model:** Built with 12 selected variables.
4. **AUC Score:** Achieved a score of 0.9492436.

### Model Evaluation

- **ROC Curve:** Plotted to determine an optimal cutoff value for the 'Conversion' target variable.
- **Cutoff Value:** Chose 0.4 based on the ROC Curve analysis.

### Conclusion

The logistic regression model, after thorough cleaning and variable selection, achieved a promising AUC score of 0.9492436. The selected cutoff value of 0.4 provides an optimal balance between sensitivity and specificity. 
