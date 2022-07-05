###############################################################################
###############################################################################
###############################################################################
## Week:5 Assignment Activity:

###############################################################################
## Installing and importing packages and libraries:

# Whole tidyverse package
library(tidyverse)
# Useful for importing data
library(readr) 
#Useful for data wrangling
library(dplyr) 
#Useful for data wrangling
library(tidyr) 
# Useful for creating tidy tables
library(knitr) 
# useful for working with vectors and functions
library(purrr)
# useful to create insightful summaries of data set
library(skimr)
# useful to create insightful reports on data set
library(DataExplorer)
# useful for visualisation
library(ggplot2)
# for pairplots
install.packages("GGally")
library(GGally)

###############################################################################
## Importing the data:

# Read the data set
games <- read.csv (file.choose (), header = T) #choose games_sales.csv

###############################################################################
## Basic checking and transforming of the data:

# Check the dataframe head
head(games)

#Check the data set structure
str(games)

# View the dataframe
View(games)

# Sense-check dataframe
as_tibble(games)

# Check dimension of dataframe
dim(games)

# Check summary of dataframe
summary(games)

##############################################################################
## Assessing the data set:

# Check for missing values
games[is.na (games)] 
sum(is.na (games))
apply(games, 2, function(x) any(is.na(x)))

#Check for duplicated values
duplicated(games)
sum(duplicated(games))

###############################################################################
##Understanding the data:

#Create DataExplorer report to understand the data
DataExplorer::create_report(games)

#The report shows no missing data, shows the distribution of the numerical 
#.. columns and shows the correlation analysis between all the variables. 

##############################################################################
## Data manipulation:

#Convert the genre column to lowercase
games$Genre = tolower(games$Genre)
head(games)

# Determine the unique values in columns Platform and Genre
unique(games$Platform)

unique(games$Genre)

# Merge the values for Genre and Platform
unite(games, Genre_Platform, c(Genre, Platform))

head(games)

###############################################################################
## Visualise the data using ggplot to understand trends between the variables:
## Q. How will you evaluate the skewness of the data?
##    i. Which plot will help you study the skewness of the data?
##    ii. If the data is skewed, is it skewed towards right or left?
## Q. What is the correlation between the two variables that will help you 
##..  predict global sales?
##    i. Which variables will you use for studying the correlation?
##    ii. What type of plot will help you visualise the correlation? 

###############################################################################
## Exploratory data analysis using qplot and ggplot to visualise the data:

# Qplotting one variable at a time to check the distribution of the data
qplot(data=games, x=Rank)

qplot(data=games, x=Platform)

qplot(data=games, x=Year)

qplot(data=games, x=Genre)

qplot(data=games, x=Publisher)

qplot(data=games, x=NA_Sales)

qplot(data=games, x=EU_Sales)

qplot(data=games, x=Global_Sales)

# Qplotting different variables together to further understand the relationship
qplot(data=games, x=NA_Sales, y=Global_Sales)

qplot(data=games, x=EU_Sales, y=Global_Sales)

qplot(data=games, x=Year, y=Global_Sales)

qplot(data=games, x=Rank, y=Global_Sales)

#Visualising the skewness of NA (North America), EU (European) and Global Sales
#.. with density plot
ggplot(games, aes(x=NA_Sales)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

ggplot(games, aes(x=EU_Sales)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

ggplot(games, aes(x=Global_Sales)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

#Plotting pairplot for Rank, NA, EU and Global Sales
ggpairs(games, columns = c('Rank', 'NA_Sales', 'EU_Sales', 'Global_Sales'))
# The NA_Sales and EU_Sales shows a correlation between them and Global_Sales.
# Before applying linear or multiple linear regression, we need to check that 
#.. predicitor variables have a linear relationship with the response variable.
#..The scatterplots and the pairplots show that there is a strong, positive,
#..linear relationship between NA and EU to Global Sales. 

###############################################################################
## Q.i. After EDA - the histogram plots show the skewness of the data best:

#Visualising the skewness of NA (North America), EU (European) and Global Sales
#.. with histograms
ggplot(games, aes(x=NA_Sales)) + 
  geom_histogram(binwidth=1, fill="green") + 
  labs(title="North America Sales Distribution", 
       x="North America Sales (millions of units)", 
       y="Count") + theme_light() +
  scale_x_continuous(breaks=seq(0, 45, 5)) +
  scale_y_continuous(breaks=seq(0, 15000, 1000))

ggplot(games, aes(x=EU_Sales)) + 
  geom_histogram(binwidth=1, fill="blue") + 
  labs(title="European Sales Distribution", 
       x="European Sales (millions of units)", 
       y="Count") + theme_light() +
  scale_x_continuous(breaks=seq(0, 35, 5)) +
  scale_y_continuous(breaks=seq(0, 16000, 1000))

ggplot(games, aes(x=Global_Sales)) + 
  geom_histogram(binwidth=1, fill="purple") + 
  labs(title="Global Sales Distribution", 
       x="Global Sales (millions of units)", 
       y="Count") + theme_light() +
  scale_x_continuous(breaks=seq(0, 85, 5)) +
  scale_y_continuous(breaks=seq(0, 13000, 1000))

## Q.ii. All of the three histogram plots show a longer tail on the right hand side of 
##.. the data distribution. Therefore the data for NA, EU and Global Sales is
##.. positively skewed. 

#Save the three visualisations for presentation use. 

###############################################################################
## Q.i. After EDA - the scatterplots the shows the correlation between the
##.. two variables that have the highest correlation to Global Sales:
##(See Correlation Analysis in Data Report - NA and GS = 0.94, EU and GS = 0.90)
## Q. ii. Scatter plots with regression lines will be best type of plot to 
##.. visualise the correlation. Pairplots (as above could also be used).

#Visualising the correlation of NA (North America) and EU (European) with
#..Global Sales with scatterplots with regression lines
ggplot(games, mapping=aes(x=NA_Sales, y=Global_Sales)) + 
  geom_point()+
  geom_smooth(col="red")+ 
  labs(title="Scatter Plot of North American Sales vs. Global Sales", 
       x="North America Sales (millions of units)", 
       y="Global Sales (millions of units)") + theme_light() +
  scale_x_continuous(breaks=seq(0, 50, 5)) +
  scale_y_continuous(breaks=seq(0, 90, 10))

ggplot(games, mapping=aes(x=EU_Sales, y=Global_Sales)) + 
  geom_point()+
  geom_smooth(col="red")+ 
  labs(title="Scatter Plot of European Sales vs. Global Sales", 
       x="European Sales (millions of units)", 
       y="Global Sales (millions of units)") + theme_light() +
  scale_x_continuous(breaks=seq(0, 30, 5)) +
  scale_y_continuous(breaks=seq(0, 90, 10))

#Save the two visualisations for presentation use. 

###############################################################################
###############################################################################
###############################################################################
## Week:6 Assignment Activity:
## Determine the predicted global sales (in millions).

###############################################################################
##Apply multi-linear regression to determine the predicted global sales for all 
##.. the video games based on: Sales of video games in North America and Europe.

# Summary statistics of the games dataframe
summary(games) 

# Pearson correlation between 2 variables
cor(games$NA_Sales, games$Global_Sales)

cor(games$EU_Sales, games$Global_Sales)

# Create a new object, specify the lm function and the variables
model1 = lm(Global_Sales ~ NA_Sales + EU_Sales, data=games)

# View the model
model1

# Print the summary statistics for the model
summary(model1) 
# The summary shows that the p values are statistically significant for both of
#.. the variables. The R squared score of 0.96 means 96% of the variance in 
#.. Global Sales can be explained by Sales in North America and Europe. The
#.. residual standard error of 0.2918 means that on average, the distance the 
#..observed values fall from the regression line is 0.2918.

# Print the confidence interval for the model
confint(model1)

# Check the distribution of the residuals
hist(residuals(model1), col = "dark red")
#The distribution is slightly skewed to the right, however it isn't abnormal
#..enough to cause major concerns.

# Checking for homoskedasticity by plotting fitted values vs residuals
plot(fitted(model1), residuals(model1))
# Add horizontal line at 0 to help interpretation
abline(h = 0, lty = 2)
# It is hard to see a pattern as there is an outlier

# Replotting witout large outlier to check for homoscedasticity
plot(fitted(model1), residuals(model1),
     xlim = c(0, 50))
# Add horizontal line at 0 to help interpretation
abline(h = 0, lty = 2)
# It appears that there is homoscedasticity.

#Calculate the VIF for each predictor variable in the model
vif(model1)
# The results 2.43 means that the variables are moderately correlated (but not
#..highly correlated and therefore this is ok.)

# Load car package
library(car)
# Perform Durbin-Watson test to check for multicolinerarity
durbinWatsonTest(model1)
#Since this statistic is very close to 2 (1.892) and the p-value (0) is less 
#..than 0.05, we can reject the null hypothesis and conclude that the residuals 
#..in this regression model are autocorrelated.

# Create a new column for the predicted Global Sales
games$Global_Sales_Predicted <- predict(model1)

#View the df to check Global Sales predicted
head(games)

# Round column to two decimal places using dplyr
games2 <- games %>%                   # Using dplyr functions
  mutate_if(is.numeric,
            round,
            digits = 2)

#View the df to check Global Sales predicted
head(games2)

# View top 30 entries of dataframe
games30 <- head(games2, 30)

# Determine your working directory
getwd()

# Set working directory
setwd("~/Desktop/LSE Data Course /Module 3 /R")

# Export top 30 results
write.csv(games30, 'games30.csv')

###############################################################################
###############################################################################