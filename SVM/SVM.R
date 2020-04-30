#SVM
#First remove all the variables in the environment
rm(list=ls())

#load data
loan_data <- read.csv('loan_data.csv')

#Structure of the data
str(loan_data)

#Summary
summary(loan_data)

#Convert the following cols to categorical using factor()
#inq.last.6mths#delinq.2yrs#pub.rec#not.fully.paid#credit.policy
loan_data$credit.policy <- factor(loan_data$credit.policy)
loan_data$inq.last.6mths <- factor(loan_data$inq.last.6mths)
loan_data$delinq.2yrs <- factor(loan_data$delinq.2yrs)
loan_data$pub.rec <- factor(loan_data$pub.rec)
loan_data$not.fully.paid <- factor(loan_data$not.fully.paid)

#----------------------------EDA--------------------------------
library(ggplot2)
#Histogram of fico.scores colored by not.fully.paid
pl <- ggplot(loan_data, aes(x=fico))
pl <- pl + geom_histogram(aes(fill=not.fully.paid), color= 'black', bins=50, alpha=0.5)
pl + scale_fill_manual(values=c('green','red')) + theme_bw()

#Barplot of purpose counts colored by not.fully.paid
pl2 <- ggplot(loan_data, aes(x=factor(purpose)))
pl2 <- pl2 + geom_bar(aes(fill=not.fully.paid) , position='dodge')
pl2 + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust =1))

#Scatterplot of fico vs int.rate
ggplot(loan_data, aes(int.rate, fico)) + geom_point() + theme_bw()
ggplot(loan_data, aes(int.rate, fico)) + geom_point(aes(color=not.fully.paid), alpha=0.4) + theme_bw()


#Building the model
#Train Test Split
library(caTools)

set.seed(201)
sample <- sample.split(loan_data$not.fully.paid, 0.7)
train <- subset(loan_data, sample==T)
test <- subset(loan_data, sample==F)

#Load the library for SVM(e1071)
library(e1071)

#Train the model using svm
model <- svm(not.fully.paid ~ ., data = train)

#Get the summary of the model
summary(model)

#Predicting
pred.values <- predict(model, test[1:13])

table(pred.values, test$not.fully.paid)

#Tuning the model
tuning.results <- tune(svm, train.x = not.fully.paid ~., data = train,
                       kernel = 'radial', ranges=list(cost=c(1,10), gamma=c(0.1,1)))

model.final <- svm(not.fully.paid ~., data=train, cost=10, gamma=0.1)
pred.values <- predict(model.final, test[1:13])
table(pred.values, test$not.fully.paid)










