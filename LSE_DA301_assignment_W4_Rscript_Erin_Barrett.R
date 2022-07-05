## Week:4 Assignment Activity

###############################################################################
## Installing and importing packages and libraries:

# Install the tidyverse and gghighlight package and import the ggplot2 library
install.packages("tidyverse") 
library(tidyverse)
library(ggplot2)
library(dplyr)
install.packages("gghighlight")
library(gghighlight)

###############################################################################
## Importing the data:

#Read the data set
lego <- read.csv(file.choose(), header = TRUE) #Choose the lego data set

###############################################################################
## Basic checking and transforming of the data:

# Check the dataframe head
head(lego)

#Check the data set structure
str(lego)

# View the dataframe
View(lego)

###############################################################################
## Visualising the data with qplot to find further insights:
## 4a. Which age group submits the most reviews?
## Notes about the data:
## According to the metadata: ages = "The age the Lego product caters to",
##.. num_reviews = "Number of reviews" "of Lego products" "customer reviews".
## The ages column isn't very explicit re: the age group - ie. for value: 10 is
##..that just age 10? or 0-10? or 10+. Because it isn't explicit, for the
##.. purpose of the assignment, I am making the assumption that for the column
##.. ages, the value is just for that value ie. 10 = 10, and not 0-10 or 10+.
## Also, as there is a value 0 - I am assuming that it is not an NA value or a
##.. missed value and that is is indeed correct and for people aged 0-1.
## Obviously very young customers are unable to write reviews, and therefore it
##.. it is assumed that an older person wrote the review on their behalf. 

# Remove the columns not needed
lego2 <- select(lego, -list_price, -piece_count, -play_star_rating, 
                -review_difficulty, -country)

# Check the new dataframe
head(lego2)

# Summary of the dataframe
summary(lego2)

#Check for NA values
apply(lego2, 2, function(x) any(is.na(x)))

#Check for duplicated values
duplicated(lego2)
sum(duplicated(lego2))

#View unique values
unique(lego2)

###############################################################################
## Extra checking and exploration of the data:

# Convert dataframe to a tibble
as_tibble(lego2)

# Use the glimpse() function
glimpse(lego2)

# Return a frequency table for the 'ages' column
table(lego2$ages)

# Return a frequency table for the 'num_reviews' column
table(lego2$num_reviews)

###############################################################################
## Exploratory data analysis using qplot and ggplot to visualise the data:

#Qplot automatic plot
qplot(ages, num_reviews, data = lego2)

#Visualise ages counts
qplot(ages, data = lego2, geom = "bar")

#Visualise num_reviews counts
qplot(num_reviews, data = lego2, geom = "bar")

#View the distribution of the data with a boxplot
qplot(factor(ages), num_reviews, data = lego2, geom = c("boxplot"))

#Scatterplot with colour coding for ages
qplot(x=ages,y=num_reviews,data=lego2, colour = ages)

#Scatterplot with colour coding for ages and jitter
qplot(x=ages,y=num_reviews,data=lego2, geom = "jitter", colour=ages)

#Scatterplot with colour coding for num_reviews
qplot(x=ages,y=num_reviews,data=lego2, colour = num_reviews)

#Scatterplot with colour coding for num_reviews and jitter
qplot(x=ages,y=num_reviews,data=lego2, geom = "jitter", colour = num_reviews)

#Create new dataframe aggregating the mean num_reviews per age group
lego3 <- aggregate( num_reviews ~ ages, lego2, mean )

#Visualising the mean with a column graph
qplot(ages, num_reviews, data = lego3, geom = "col")

#Visualising the mean with a dot plot
lego_mean <-ggplot(lego3, aes(x=ages, y=num_reviews)) + 
  geom_dotplot(binaxis='y', stackdir='center')

lego_mean

#Visualising the mean with a lollipop plot
ggplot(lego3, aes(x=ages, y=num_reviews)) +
  geom_point() + 
  geom_segment( aes(x=ages, xend=ages, y=0, yend=num_reviews))

#Create new data frame aggregating the median num_reviews per age group
lego4 <- aggregate( num_reviews ~ ages, lego2, median )

#Visualising the median with a column graph
qplot(ages, num_reviews, data = lego4, geom = "col")

#Visualising the mean with a dot plot
lego_med <-ggplot(lego4, aes(x=ages, y=num_reviews)) + 
  geom_dotplot(binaxis='y', stackdir='center')

lego_med

#Visualising the median with a lollipop plot
ggplot(lego4, aes(x=ages, y=num_reviews)) +
  geom_point() + 
  geom_segment( aes(x=ages, xend=ages, y=0, yend=num_reviews))

#Mutate ages from numeric to categorical
lego2$ages<-as.factor(lego2$ages)
str(lego2)

#Visualising the mutated dataframe: column, boxplot and violin
qplot(x=ages,y=num_reviews,data=lego2, geom = "col")

qplot(x=ages,y=num_reviews,data=lego2, geom = "boxplot")

qplot(x=ages,y=num_reviews,data=lego2, geom = "violin")

#Creating new dataframe for aggregated total of num_reviews for each age
legocounts <- lego2 %>% group_by(ages) %>% summarise(num_reviews = sum(num_reviews))

#View new dataframe
legocounts

#Visualise total
qplot(x = ages, y = num_reviews, data = legocounts, geom = "col")

###############################################################################
##After EDA - the four visualisations going to use: box plot, mean, median 
##.. and total.
##Visualisations with ggplot adding labels, titles and adjusting theme for 
##.. presentation use. 

#Boxplot visualisation:
ggplot(lego2, aes(ages, num_reviews)) + 
  geom_boxplot(fill="plum") + 
  labs(subtitle="Number of Reviews grouped by Age",
       x="Age",
       y="Number of Reviews")+ theme_light() +
  scale_y_continuous(breaks=seq(0, 400, 25))

#Mean visualisation:
ggplot(lego3, aes(x=ages, y=num_reviews)) +
  geom_segment( aes(x=ages, xend=ages, y=0, yend=num_reviews), color="black") +
  geom_point( color="red", size=3) +
  scale_x_continuous(breaks=seq(0,30,1), limits=c(0,30)) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("Age") +
  ylab("Mean Number of Reviews") +
  ggtitle("Lego Products: Mean Number of Reviews by Age")

#Median visualisation:
ggplot(lego4, aes(x=ages, y=num_reviews)) +
  geom_segment( aes(x=ages, xend=ages, y=0, yend=num_reviews), color="black") +
  geom_point( color="orange", size=3) +
  scale_x_continuous(breaks=seq(0,30,1), limits=c(0,30)) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("Age") +
  ylab("Median Number of Reviews") +
  ggtitle("Lego Products: Median Number of Reviews by Age")

#Totals visualisation:
ggplot(legocounts, aes(x=ages, y=num_reviews)) + 
  geom_bar(stat="identity", width=.8, fill="#0e668b") + 
  labs(title="Lego Products: Total Number of Reviews by Age", 
       x="Age", 
       y="Total Number of Reviews") + theme_light() +
  scale_y_continuous(breaks=seq(0, 50000, 5000))

#Save the four visualisations for presentation use. 

################################################################################
################################################################################
##What is the most expensive lego set purchased by customers who are at least 
##.. 25 years old (>25 years)?
## Notes about the data:
## According to the metadata: ages = "The age the Lego product caters to",
##.. It is not a confirmation that someone of that age purchased the product.
##.. Therefore it is an assumption that the age the Lego product caters to,
##.. is the age of the customer who purcahsed that specific product.

# Subset the data
data_age25 <- lego[lego$age>=25,]

# Check the new dataframe
head(data_age25)

# Summary of the dataframe
summary(data_age25)

#Check for NA values
apply(data_age25, 2, function(x) any(is.na(x)))

###############################################################################
## Extra checking and exploration of the data:

# Convert dataframe to a tibble
as_tibble(data_age25)

# Use the glimpse() function
glimpse(data_age25)

###############################################################################
## Exploratory data analysis using qplot to visualise the data:

#Visualise ages
qplot(ages, data= data_age25)

#Visualise list_price with histogram
qplot(list_price, data = data_age25)

#Visualise ages vs. list_price for >=25 years with point plot
qplot(ages, list_price, data = data_age25, geom = "point")

#Visualise ages vs. list_price for >=25 years with point plot with jitter
qplot(ages, list_price, data = data_age25, geom = "jitter")

#Visualise ages vs. list_price for >=25 years with box plot
qplot(x=ages, y=list_price, data=data_age25, geom = "boxplot")

#Visualise list_price with dotplot
c <- ggplot(data_age25, aes(list_price)); c2 <- ggplot(data_age25)
c + geom_dotplot(binwidth = 5, dotsize = .25) + 
  coord_cartesian(xlim = c(0, 280), ylim = c(0, 200))

#Visualise list_price with frequency line graph
c + geom_freqpoly()

#Visualise list_price with frequency histogram
c + geom_histogram(binwidth = 10)

#Visualise list_price with area graph
c + geom_area(stat = "bin")

#Visaulise list_price with density graph
c + geom_density(kernel = "gaussian")

#Visualise scatter plot with more jitter
ggplot(data_age25, aes(ages, list_price))+
  geom_jitter(width = 25, height = 0) + 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

#Mutate ages from numeric to categorical
data_age25$ages<-as.factor(data_age25$ages)
str(data_age25)

#Visualising the mutated dataframe: boxplot and violin
qplot(x=ages,y=list_price,data=data_age25, geom = "point")

qplot(x=ages,y=list_price,data=data_age25, geom = "boxplot")

#Subset the dataframe to the top price bracket >200
data_age25_top <-filter(data_age25, list_price > 200)

#Visualising products for list_price >200 with jitter
qplot(ages, list_price, data = data_age25_top, geom = "jitter")

###############################################################################
##After EDA - the four visualisations going to use: box plot and point plot.
##Visualisations with ggplot adding labels, titles and adjusting theme for 
##.. presentation use. 

#Boxplot visualisation:
ggplot(data_age25, aes(ages, list_price)) + 
  geom_boxplot() +
  geom_point(data = data_age25 %>% 
               filter(list_price > 259),
                      pch=21, 
                      size=4,
                      colour="red") +
  geom_label(
    data=data_age25 %>% filter(list_price>259), # Filter data first
  aes(label=list_price, x=6, y=260)) +
  labs(subtitle="Most expensive Lego set purchased by customers: 25 years and over",
       x="Age",
       y="List Price")+ theme_light() +
  scale_y_continuous(breaks=seq(0, 300, 20))

#Scatterplot visualisation
ggplot(data_age25, aes(ages, list_price))+
  geom_jitter(width = 0.05, col = "red") +
  gghighlight(list_price > 259, 
              use_direct_label = TRUE, 
              label_key = list_price,
              label_params = list(size = 2)) +
  labs(subtitle="Most expensive Lego set purchased by customers: 25 years and over",
       x="Age",
       y="List Price")+ theme_light() +
  scale_y_continuous(breaks=seq(0, 300, 20))

#Save the two visualisations for presentation use.

###############################################################################
###############################################################################













