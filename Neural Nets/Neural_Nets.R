#Neural Nets
#First, remove all environment variables
rm(list=ls())


#Load data
df <- read.csv('bank_note_data.csv')

#check the head of the data frame
head(df)

#Structure of the df
str(df)

#Summary of the df
summary(df)

#Missing Value Analysis
apply(df, 2, function(x){sum(is.na(x))})
#Perfect!!! There are no missing values in the dataset and the data is normalized too.

#------------------------ EDA ------------------------------------
library(ggplot2)

#Image.Var vs Image.Skew
ggplot(df, aes(Image.Var, Image.Skew)) + geom_point(aes(color=Class))

#Train Test Split
library(caTools)

set.seed(202)

split <- sample.split(df$Class, SplitRatio = 0.75)
#Train data
train <- subset(df, split==T)
#Test data
test <- subset(df, split==F)

#Check the structure of the train and test data
str(train)
str(test)

#As we can see that Class is still a numeric information, bcoz
#Neural Nets required numeric information only

#Building Neural Net
install.packages('neuralnet')
library(neuralnet)

#Use neuralnet function to train a neural net, set linear.output=F &
# Use 8 hidden neurons(hidden=8)
nn <- neuralnet(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy ,
                data = train, hidden = 8, linear.output = FALSE)
#We need to mention the features which we want to pass to neural net
#It is not same as other normal algorithms

#Predictions
predicted.nn.vals <- compute(nn, test[, 1:4])

#Head of the predicted values
head(predicted.nn.vals$net.result)

#Apply round function to the predicted values. so we only see 0s & 1s instead of all the decimal values
preds <- sapply(predicted.nn.vals$net.result, round)

head(preds)

#Confusion Matrix
table(preds, test$Class)

#Finally, Neural Net did an excellent job!!!

#Compare with RandomForest
library(randomForest)

df$Class <- factor(df$Class)
library(caTools)

set.seed(200)
split.random <- sample.split(df$Class, SplitRatio = 0.75)

#Train data
train.rf <- subset(df, split==T)
#Test data
test.rf <- subset(df, split==F)

#Creating a RandomForest model with new adjusted training data
model.rf <- randomForest(Class ~ ., data = train.rf)

#Predictions of RandomForest model
rf.preds <- predict(model.rf, test$Class)

#Confusion Matrix
table(rf.preds, test$Class)

#We can clearly see that Neural Net is perfectly better than RandomForest
#But RandomForest also did its job well!!!


















