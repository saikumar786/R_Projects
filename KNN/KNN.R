#KNN
#simple supervised algorithm can be used for both Reg & Classification.
#Mostly used for classification problems

#Get the Data
iris_data <- read.csv('iris.csv')

#Head of the df
head(iris_data)

#structure of the data
str(iris_data)

#The Iris data set has all features in the same order of magnitude,
#But it is a good practice to standardize while using KNN as it uses distance
#method

#Standardizing Data
std.vars <- scale(iris_data[1:4])
std.vars
#Check the scaling worked by checking the variance of one of the cols
var(std.vars[,1])

#Join the standardized data with response col
final.data <- cbind(std.vars, iris_data[5])

head(final.data)

#Train and Test Splits
library(caTools)
set.seed(201)

sample <- sample.split(final.data$species, SplitRatio = 0.7)
train <- subset(final.data, sample==TRUE)
test <- subset(final.data, sample==FALSE)

#Building Model
#for KNN model
library(class)

#Applying model
pred.vals <- knn(train[1:4], test[1:4], train$species, k=1)
pred.vals

#Misclassification Rate
mis_rate <- mean(test$species != pred.vals)

#Choosing K value
#Creating a plot of the error rate for k values ranging from 1-10
pred.vals <- NULL
error <- NULL

for(i in 1:10){
  set.seed(201)
  pred.vals <- knn(train[1:4], test[1:4], train$species, k=i)
  error[i] <- mean(test$species != pred.vals)
}

library(ggplot2)

k.values <- 1:10
error.df <- data.frame(error, k.values)

#Plot
plt <- ggplot(error.df, aes(x=k.values, y=error)) + geom_point()
plt + geom_line(lty='dotted', color='red')

