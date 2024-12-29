# ISTA 321 Project Report

## Authors:
- Claire Baker
- Sherif Shawashen

## Introduction

For our Project, we were tasked with analyzing and applying data mining techniques to the dataset, “Life Expectancy (WHO)”, a statistical analysis of factors influencing life expectancy. The dataset, compiled by the World Health Organization, contains 2938 observations of 22 variables. We focused on cleaning the dataset, making the variable names more cohesive, and removing "NA" values using R's `na.omit` function.

To ensure efficient data analysis, we created several functions for:
- Visualizing residual plots
- Visualizing linear regression
- Visualizing the confidence interval

We also considered the presence of a "year" column, which could affect the analysis due to potential time-based trends.

## Research Question #1

**Research Question:** What is the relationship between consumption of alcohol and life expectancy?

We hypothesized a positive relationship between alcohol consumption and life expectancy. Using linear regression, we compared the variables “life expectancy” and “alcohol”. The summary statistics suggested that when alcohol consumption is zero, life expectancy is 64.76 years. For each unit increase in alcohol consumption, life expectancy increased by 0.87925 years. Both p-values for the intercept and alcohol were less than 0.05, indicating a statistically significant relationship.

Despite the positive relationship indicated by the regression line, the scatterplot of data points showed a wide dispersion, suggesting a weak relationship. The model's R-squared value of 0.1622 shows that only 16.22% of the variance in life expectancy is explained by alcohol consumption. We also visualized the confidence intervals, confirming that the relationship is statistically significant.

## Research Question #2

**Research Question:** What is the relationship between the number of years in schooling and GDP per capita?

We hypothesized a positive relationship between years of schooling and GDP per capita. A linear regression model was created comparing the variables “GDP” and “schooling”. The results indicated that for every unit increase in schooling, GDP increased by $1921.10. The p-values for both GDP and schooling were less than 0.05, indicating statistical significance. However, the model's R-squared value of 0.219 suggests that only 21.9% of the variance in GDP can be explained by schooling.

The residuals indicated heteroscedasticity, suggesting that this model may not best represent the data, especially for lower levels of schooling. Visualizations showed that the relationship between schooling and GDP is not perfectly linear, and the residuals increased significantly as years of schooling increased.

## Research Question #3

**Research Question:** Does a country’s GDP, years of schooling, status, or total expenditure affect life expectancy?

We created a multiple regression model with GDP, schooling, status, and total expenditure as predictors. The results suggested that GDP and schooling have a statistically significant effect on life expectancy. Total expenditure, however, was not statistically significant. The R-squared value of 0.5449 indicates that our model explains 54.49% of the variance in life expectancy. 

Further analysis revealed an interaction between GDP and total expenditure, where the positive effect of total expenditure on life expectancy decreases as GDP increases. This suggests that the impact of total expenditure on life expectancy is more pronounced in lower GDP countries.

## Conclusion

This report presents our findings from analyzing the "Life Expectancy (WHO)" dataset using data mining techniques. Our analysis led to the following conclusions:
- Alcohol consumption has a positive but weak relationship with life expectancy.
- Years of schooling positively impact GDP per capita, but the model has limitations.
- Both GDP and total expenditure significantly affect life expectancy, with an interaction effect between the two.
