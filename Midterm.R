# Midterm Data

# Possible Research Questions:
# Is there a relationship between country status and life expectancy?
# Does Hepatitis B rate or Measles affect life expectancy trends in a certain country more or less?
library(tidyverse)

setwd("/Users/clairebaker/Downloads/")
WHO<-read.csv("life_expectancy_data.csv")
head(WHO,10)

dim(WHO)
str(WHO)
#Every column up to BMI I will explore, every column after BMI Sherif will explore

#Objectives when exploring: Knowing the mean, median, ggplot visualizations, testing, see if there are unique values, find any outliers, max and min

#Check for missing values
missing_values <- colSums(is.na(WHO))
print(missing_values)

#Handle missing values: remove rows with missing values
#Use this instead of WHO for bigger picture data collection
who_data_clean <- na.omit(WHO)

###Country
#193 countries are being measured
unique(WHO$Country)
class(WHO$Country)
#How many times is each country mentioned
country_counts <- WHO %>%
  count(Country)
print(country_counts)

###Year
#Summary Statistics
summary(WHO$Year)
class(WHO$Year)

###Status
#Summary Statistics
summary(WHO$Status)
class(WHO$Status)

#How many countries are developed versus developing
status_counts <- WHO %>%
  count(Status)
print(status_counts)
#There are 512 developed countries and 2426 developing countries, here is a visualization
ggplot(status_counts, aes(x = Status, y = n, fill = Status)) +
  geom_bar(stat = "identity") +
  labs(title = "Count of Countries by Status",
       x = "Status",
       y = "Count") +
  theme_minimal()

###Life expectancy
#Summary Statistics
summary(WHO$Life.expectancy)
class(WHO$Life.expectancy)
#Visualization
WHO %>%
  filter(is.na(Life.expectancy)) %>%
  count()
summary(WHO$Life.expectancy)
hist(WHO$Life.expectancy, main="Histogram of Life Expectancy", xlab="Life Expectancy")

#Calculate mean life expectancy for each year
mean_life_expectancy <- WHO %>%
  group_by(Year) %>%
  summarize(Mean_Life_Expectancy = mean(Life.expectancy, na.rm = TRUE))
print(mean_life_expectancy)
#Life expectancy has been slowly rising from 2000-2015
#Histogram
ggplot(mean_life_expectancy, aes(x = Year, y = Mean_Life_Expectancy)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Mean Life Expectancy by Year",
       x = "Year",
       y = "Mean Life Expectancy")

###Adult mortality
#Summary Statistics
summary(WHO$adult.mortality)
class(WHO$Adult.Mortality)
hist(WHO$Adult.Mortality, main="Histogram of Adult Mortality", xlab="Adult Mortality Rate")

###Infant deaths
#Summary Statistics
summary(WHO$infant.deaths)
class(WHO$infant.deaths)
hist(WHO$infant.deaths, main="Histogram of Infant Deaths", xlab="Infant Deaths")


###Alcohol
#Summary Statistics
summary(WHO$Alcohol)
class(WHO$Alcohol)
hist(WHO$Alcohol, main="Histogram of Alcohol Consumption", xlab="Alcohol Consumption (in litres)")


###Percentage Expenditure
#Summary Statistics
summary(WHO$percentage.expenditure)
class(WHO$percentage.expenditure)
hist(WHO$percentage.expenditure, main="Histogram of Percentage Expenditure", xlab="Percentage Expenditure (% of GDP per capita)")


###Hepatitis B
#Summary Statistics
summary(WHO$Hepatitis.B)
class(WHO$Hepatitis.B)
hist(WHO$Hepatitis.B, main="Histogram of Hepatitis B", xlab="Hepatitis B immunization coverage among 1-year-olds (%)")



###Measles
#Summary Statistics
summary(WHO$Measles)
class(WHO$Measles)
hist(WHO$Measles, main="Histogram of Measles", xlab="Measles - number of reported cases per 1000 population")


###BMI
#Summary Statistics
summary(WHO$BMI)
class(WHO$BMI)
hist(WHO$BMI, main="Histogram of BMI", xlab="Average Body Mass Index of entire population")

###What is the relationship between the number of years in schooling and GDP per capita?
schooling_gdp <- na.omit(WHO[, c("Schooling", "GDP")])
model <- lm(GDP ~ Schooling, data = schooling_gdp)
summary(model)
ggplot(schooling_gdp, aes(x = Schooling, y = GDP)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Relationship between Schooling and GDP per Capita",
       x = "Number of Years in Schooling",
       y = "GDP per Capita")
#Model Diagnostics
qqnorm(resid(model), main = "Q-Q Plot for Normality of Residuals")
qqline(resid(model), col = "red")
#In this Q-Q plot, the residuals deviate from the QQ line, indicating a violation of normality.
plot(model, which=1)
#The residuals vs fitted: The model shows Heteroskedasticity refers to a situation where the variance of the residuals is unequal over a range of measured values. If heteroskedasticity exists, the population used in the regression contains unequal variance, the analysis results may be invalid.
plot(model, which=3)
#Scale-location: Residuals are not spread equally across the range of fitted values. 
plot(model, which=4)
#Cook's distance: some of the points have a lot more impact on the data than others.

#Slope (1882.4): This is the key coefficient that indicates the relationship between schooling and GDP. For every additional year of schooling, the GDP per capita is estimated to increase by about $1,882.4. This positive slope suggests a strong positive relationship between schooling and GDP.
#P-value (< 2e-16): The p-value associated with both the intercept and the schooling coefficient is extremely small (much less than 0.001), meaning that the relationship between years of schooling and GDP is statistically significant. The stars (***) also indicate high statistical significance.
#The R-squared value indicates how well the independent variable (schooling) explains the variability in the dependent variable (GDP). Here, R-squared = 0.2009, meaning that approximately 20.09% of the variation in GDP is explained by the number of years of schooling. While 20% isn't a very high value, it suggests that schooling is an important but not the only factor affecting GDP.

###Predicting life expectancy using immunization coverage among 1-year-olds (%) for Hepatitis B, Diphtheria, Polio
diseases <- na.omit(WHO[, c("Life.expectancy", "Hepatitis.B", "Diphtheria", "Polio")])
model2 <- lm(Life.expectancy ~ Hepatitis.B + Diphtheria + Polio, data = diseases)
summary(model2)
ggplot() +
  geom_point(data = diseases, aes(x = Hepatitis.B, y = Life.expectancy), color = "blue", alpha = 0.6) +
  geom_smooth(data = diseases, aes(x = Hepatitis.B, y = Life.expectancy), method = "lm", se = FALSE, color = "blue") +
  geom_point(data = diseases, aes(x = Diphtheria, y = Life.expectancy), color = "green", alpha = 0.6) +
  geom_smooth(data = diseases, aes(x = Diphtheria, y = Life.expectancy), method = "lm", se = FALSE, color = "green") +
  geom_point(data = diseases, aes(x = Polio, y = Life.expectancy), color = "red", alpha = 0.6) +
  geom_smooth(data = diseases, aes(x = Polio, y = Life.expectancy), method = "lm", se = FALSE, color = "red") +
  labs(title = "Life Expectancy vs Immunization Coverage",
       x = "Immunization Coverage (%)",
       y = "Life Expectancy (Years)")
library(car)
vif(model2)
#The vif (Variance Inflation Factor) indicates that these is little multicollinerity between the predictors
#Diphtheria and Polio immunization have a significant positive effect on life expectancy, with both p-values being < 2e-16. This means we can confidently say that higher immunization coverage for Diphtheria and Polio is associated with an increase in life expectancy.
#The R-squared value indicates that 16.49% of the variation in life expectancy can be explained by the immunization coverage for Hepatitis B, Diphtheria, and Polio.


#Whether there is an interaction effect between a country’s development status and GDP on life expectancy
WHO$Status <- as.factor(WHO$Status)
gdpstatus <- na.omit(WHO[, c("Life.expectancy", "GDP", "Status")])
model3 <- lm(Life.expectancy ~ Status * GDP, data = gdpstatus)
summary(model3)
ggplot(gdpstatus, aes(x = GDP, y = Life.expectancy, color = Status)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", aes(linetype = Status), se = FALSE) +
  labs(title = "Interaction Effect of Status and GDP on Life Expectancy",
       x = "GDP per Capita",
       y = "Life Expectancy (Years)")
#Interaction Effect: It shows that for Developing countries, the effect of GDP on life expectancy is 0.0003416 years per unit increase in GDP, in addition to the base effect of GDP in Developed countries.
#Therefore, the total effect of GDP on life expectancy for Developing countries is 0.00006206 + 0.0003416 = 0.00040366 (or about 0.4 years for every $1,000 increase in GDP).
#This interaction is highly significant with a p-value < 2e-16, meaning the effect of GDP on life expectancy is different in Developing countries compared to Developed countries.
#Slope: 
#P value: 2.2e-16. Extremely small o value, indicating that there is an interaction effect between a country's development status and GDP on life expectancy.
#Multiple R2: This tells us that about 35.37% of the variance in life expectancy is explained by the model, which includes development status, GDP, and their interaction. While this is a moderate fit, it also suggests other factors besides GDP and development status influence life expectancy.

spain_data <- subset(WHO, Country == "Spain")
spain_clean <- na.omit(spain_data[, c("Life.expectancy", "GDP", "Schooling", "Total.expenditure")])
model_spain <- lm(Life.expectancy ~ GDP + Schooling + Total.expenditure, data = spain_clean)
summary(model_spain)
#p value: p value is 0.7817, which is higher than .05, and the three p values of the predictors are higher than .05, indicating that None of the predictors (GDP, Schooling, or Total expenditure) have p-values below 0.05, meaning none of them have a statistically significant effect on life expectancy in Spain according to this model.
#The predictors do not accurately predict life expectancy in Spain