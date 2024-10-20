library(ggplot2)
library(dplyr)
library(MASS)


#What is the relationship between consumption of alcohol and life expectancy?
linearRegressionModel <- lm(life_expectancy ~ alcohol, data=WHO)
# the R-squared is .16 which mean that the modeal is bad
# p-values is less than .05 which is 

# even when we Overfit model using a polynomial (degree 5) the  R-squared increases to .17 which is not effecient 
linearReg <- lm(life_expectancy ~ poly(alcohol, 5), data=WHO)
summary(linearReg)
summary(linearRegressionModel)

# calculate confidence intervals for the model
confint(linearRegressionModel)

# alcohol consumption vs life expectancy
  ggplot(WHO, aes(y = life_expectancy, x = alcohol)) +
    geom_point() +
    theme_minimal() +
    labs(title = "Alcohol Consumption vs Life Expectancy",
         x = "Alcohol",
         y = "Life Expectancy")
  
 # Predict life expectancy based on alcohol consumption using the linear model and
  #add it as a column in the data fram 
  
  WHO$predicted_life <- predict(linearRegressionModel)
  
  # Visualize residuals
  ggplot(WHO, aes(x = alcohol, y = life_expectancy)) +
    geom_point() +
    geom_segment(aes(xend = alcohol, yend = predicted_life), color = "red") +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    theme_minimal() +
    labs(title = "Visualizing Residuals",
         x = "alcohol",
         y = "life_expectancy")
  
  
  # Visualize the linear relationship 
  ggplot(WHO, aes(x = alcohol, y = life_expectancy)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    theme_minimal() +
    labs(title = "Visualizing Residuals",
         x = "alcohol",
         y = "life_expectancy")


#What is the relationship between the number of years in schooling and GDP per capita?

  
  gdpSchoolingModel <- lm(gdp ~ schooling, data=WHO)
  # the R-squared is .219 which mean that the modeal is bad
  
  # even when we Overfit model using a polynomial (degree 5) the  R-squared increases to .28 which is not effecient 
  gdpSchoolingModelPoly <- lm(gdp ~ poly(schooling, 5), data=WHO)
  
  summary(gdpSchoolingModelPoly)
  
  summary(gdpSchoolingModel)
  confint(gdpSchoolingModel)
  
  
  
  ggplot(WHO, aes(y = gdp, x = schooling)) +
    geom_point() +
    theme_minimal() +
    labs(title = "Schooling vs GDP",
         x = "Schooling",
         y = "GDP")
  
  
  ggplot(WHO, aes(x = schooling, y = gdp)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    theme_minimal() +
    labs(title = "Visualizing Residuals",
         x = "Schooling",
         y = "GDP")
  
  
  WHO$predictedGdp <- predict(gdpSchoolingModel)
  
  ggplot(WHO, aes(x = schooling, y = gdp)) +
    geom_point() +
    geom_segment(aes(xend = schooling, yend = predictedGdp), color = "red") +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    theme_minimal() +
    labs(title = "Visualizing Residuals",
         x = "Schooling",
         y = "GDP")
  
  
 # Either percentage expenditure or total expenditure and how it predicts life expectancy

  totalExpenditureModel <- lm(life_expectancy ~ total_expenditure, data=WHO)
  # the R-squared is .16 which mean that the modeal is bad
  # p-values is less than 
  
  # even when we Overfit model using a polynomial (degree 5) the  R-squared increases to .17 which is not effecient 
  linearReg <- lm(life_expectancy ~ poly(total_expenditure, 5), data=WHO)
  summary(linearReg)
  summary(totalExpenditureModel)
  confint(totalExpenditureModel)
  
  
  
  ggplot(WHO, aes(y = life_expectancy, x = total_expenditure)) +
    geom_point() +
    theme_minimal() +
    labs(title = "Alcohol Consumption vs Life Expectancy",
         x = "Alcohol",
         y = "Life Expectancy")
  
  ggplot(WHO, aes(x = total_expenditure, y = life_expectancy)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    theme_minimal() +
    labs(title = "Visualizing Residuals",
         x = "alcohol",
         y = "life_expectancy")
  
  
  WHO$predicted_life <- predict(linearRegressionModel)
  
  ggplot(WHO, aes(x = total_expenditure, y = life_expectancy)) +
    geom_point() +
    geom_segment(aes(xend = total_expenditure, yend = predicted_life), color = "red") +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    theme_minimal() +
    labs(title = "Visualizing Residuals",
         x = "alcohol",
         y = "life_expectancy")
  
  
  
 
