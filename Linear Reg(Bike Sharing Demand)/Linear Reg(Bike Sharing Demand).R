#Read in the bikeshare.csv file and set to a dataframe
bike <- read.csv('bikeshare.csv')

#Check the head of the dataframe bike
head(bike)
#We can see clearly that 'count' is the target variable we are trying to predict

#Exploratory Data Analysis
#Need to load ggplot2 package
library(ggplot2)
#Scatter plot of 'count' vs 'temp'
ggplot(bike, aes(temp, count)) + geom_point(alpha=0.2, aes(color=temp)) + theme_bw()


#Plot count vs datetime as scatterplot with color gradient based on temp.
#Need to convert datetime col into POSIXct before plotting
bike$datetime <- as.POSIXct(bike$datetime)

ggplot(bike, aes(datetime,count)) + geom_point(aes(color=temp), alpha=0.5) + scale_color_continuous(low="#55D8CE", high='#FF6E2E') + theme_classic()

#Calculating the correlation between temp, season and count
cor(bike[,c('temp','season','count')])

#Now, explore season data
ggplot(bike, aes(factor(season), count)) + geom_boxplot(aes(color=factor(season))) + theme_grey()

#Feature Engineering
#Create an hour col that takes hour from thhe datetime col.
bike$hour <- sapply(bike$datetime, function(x){format(x, "%H")})
head(bike)

#Scatter plot of count versus hour
ggplot(bike, aes(hour, count)) + geom_point(aes(color=temp)) + theme_bw()

#Scatter plot of count versus hour with some additional data(Count on workingday=1)
library(dplyr)
plot1 <- ggplot(filter(bike,workingday==1), aes(hour,count))
plot1 <- plot1 + geom_point(position=position_jitter(w=1, h=0), aes(color=temp), alpha=0.5)
plot1 <- plot1 + scale_color_gradientn(colours = c('dark blue', 'blue', 'light blue', 'light green',
                                                   'yellow', 'orange', 'red'))
plot1 + theme_bw()

#Scatter plot of count versus hour with some additional data(Count on workingday=0 means not working day)
plot2 <- ggplot(filter(bike,workingday==0), aes(hour,count))
plot2 <- plot2 + geom_point(position=position_jitter(w=1, h=0), aes(color=temp), alpha=0.5)
plot2 <- plot2 + scale_color_gradientn(colours = c('dark blue', 'blue', 'light blue', 'light green',
                                                   'yellow', 'orange', 'red'))
plot2 + theme_bw()

###############Building a model##############################
#model with only 1 feature(temp)
temp.model <- lm(count~temp, bike)

#Summary of the model
summary(temp.model)

#Calculating that how many bike rentals would this model predict if temp=25C
#Method 1
6.042 + 9.1705 * 25

#Method 2
temp.test <- data.frame(temp=c(25))
predict(temp.model, temp.test)

#Convert the hour column to a column of numeric values
bike$hour <- sapply(bike$hour, as.numeric)

#Now, build a model that predicts count based off of the following features
#season,holiday, workingday, weather, temp, humidity, windspeed, hour
model.final <- lm(count ~ -casual -registered -datetime -atemp, bike)
#Summary
summary(model.final)


#A linear model like the one we chose which uses OLS won't be ablt to take into account seasonality
#of our data. Even, we can see from the summary of the model that it's not performing well!





