# Midterm Data

# Possible Research Questions:
# Is there a relationship between country status and life expectancy?
# Is there a relationship between healthcare expenditure and life expectancy?*****
# Do different regions exhibit varying patterns in life expectancy trends?
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


