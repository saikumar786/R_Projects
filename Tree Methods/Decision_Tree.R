
# load data
train<- read.csv("DE_train.csv")
test <-read.csv("DE_test.csv")

# 1. Univariate Analysis - depends on whether the variable type is categorical or continuous.
# exploring all variables one by one.
str(train)
library(mlr)
summarizeColumns(train) # gives a much comprehensive view of the data set as compared to base str() function

# out of 12 variables, there are 3 continuous (int) variables and 9 categorical (Factor) variables
# where the last one (Income.Group) is the outcome

# Lets group categorical and continous variables
train_cont <- subset(train, select=c(ID, Age, Hours.Per.Week))
train_cat <- subset(train, select=-c(ID, Age, Hours.Per.Week))

# Continous variable
# focus on calculating measure of central tendency and spread of data such as Mean, Median, Range, IQR and Standard Deviation.
summary(train_cont)

# For a detailed version of this summary, let's use a package named 'pastecs'
# pastecs: Package for Analysis of Space-Time Ecological Series:

#install.packages("pastecs")
library(pastecs)

# Set significant digits and get detailed summary
options(scipen = 100)
options(digits = 2)
stat.desc(train_cont)

# nbr.val - shows number of values in a variable 
# nbr.null - shows number of null(missing) values 
# nbr.na - shows number of NA(missing) values 
# CI.mean.0.95 - considers 95% confidence interval

# Categorical Variables:
# we generally use frequency table to understand distribution of each category. 
# It can be measured using two metrics, Count and Count%

# check the number of unique values in each categorical variable.
apply(train_cat, 2, function(x){length(unique(x))}) # 1- for row and 2 for column

# Print the counts of each of each category
table(train_cat$Race)

# Print the percentage of observation in each category
# prop.table - prints the proportions of table
as.matrix(prop.table(table(train_cat$Race)))

# Print the counts of top 20 countries
head(sort(table(train_cat$Native.Country), decreasing = TRUE), 20)

# Print the percentage of observations of top 20 countries
head(round(sort(prop.table(table(train_cat$Native.Country)), 
                decreasing = TRUE), 4), 20)

# Multivariate Analysis
# Both categorical
#install.packages(gmodels)
library(gmodels) # gives cross table function from gmodels

# compare sex and income group.
CrossTable(train$Sex, train$Income.Group)

library(ggplot2)
# analyze the stats for males now
# plot stacked bar chart
ggplot(train, aes(Sex, fill=Income.Group))+geom_bar()+
  labs(title = "Stacked Bar Chart ", x="Sex", y="Count")+
  theme_light()

# create box plot
ggplot(train, aes(Sex, Hours.Per.Week))+
  geom_boxplot()+labs(titles="Boxplot")

# Missing value treatment
#check the number of missing vales in complete train data set
table(is.na(train))

#check the number of missing vales column wise
colSums(is.na(train)) 

#check the number of missing vales in complete test data set
table(is.na(test))

#check the number of missing vales column wise
colSums(is.na(test))

# We found missing values in categorical data, so impute them mode values
# For missing value imputation, mlr packages is one of multi-tasking and powerful package.

#install.packages("mlr")
library(mlr)

# impute missing values with mode
imputed_data <- impute(train, 
                classes=list(factor=imputeMode()))

# update train data set with imputed values
train <- imputed_data$data

# Now, check the missing values
colSums(is.na(train))


# impute missing values with mode
imputed_test_data <- impute(test, 
                    classes=list(factor=imputeMode()))

# update train data set with imputed values
test <- imputed_test_data$data

# Now, check the missing values
colSums(is.na(test))

# We can check outliers in continuous variables by creating simple scatter plots.
library(ggplot2)

# create scatter plot
ggplot(train, aes(ID, Age))+geom_jitter()

# scatter plot for Hours Per Week
ggplot(train, aes(ID, Hours.Per.Week))+geom_jitter()

# Variable Transformation
# check the class of all variables
sapply(train, class) # sapply returns a vector, similar to lapply

# determine the percentage of observation in each category
as.matrix(prop.table(table(train$Workclass)))

# we can combine the categories with very few observations
# lets combine categories with less than 5% of the values

library(car) # Companion to Applied Regression

# combining factor levels with few observations in a new level named Others
train$Workclass <- recode(train$Workclass, 
                   "c('State-gov','Self-emp-inc', 'Federal-gov', 
                   'Without-pay', 'Never-worked')='Others'")
test$Workclass <- recode(test$Workclass, "c('State-gov',
                  'Self-emp-inc', 'Federal-gov', 
                         'Without-pay', 'Never-worked')='Others'")

# lets check the factor level percentages now
as.matrix(prop.table(table(train$Workclass)))


# Data Preprocessing
# Encode dependent variable in both train and test datasets
# "<=50K" - This will be converted to 0
# ">50K" - This will be converted to 1

train$Income.Group <- ifelse(train$Income.Group == "<=50K", 0,1)

# check if levels are encoded
table(train$Income.Group)

# remove identifier variable from data
train <- subset(train, select = -c(ID))

# Building decision tree model

# install rpart package and load it
#install.packages("rpart")
library("rpart")

set.seed(333)
train.tree <- rpart(Income.Group ~ ., data = train, 
                    method = "class", 
                    control = rpart.control(minsplit = 20, 
                                            minbucket = 100, 
                                            maxdepth = 10), 
                                            xval=5)
# minsplit - refers to min number of observations which must exist in a node to split
# minbucket - refers to min number of observations which must exist in terminal nodes(leaf)
# maxdepth - refers to depth of the tree
# xval - refers to cross validation

# Summary of the model
summary(train.tree)

# plot the tree
#install.packages(rpart.plot)
library(rpart.plot)

rpart.plot(train.tree)

#Interpretations: 
# 1. Relationship is the most important variable. 
# 2. The first node: If the relationship status is 'Not in family', 'Own child', 'Unmarried', 'Other relatives', the tree predicts their salary <= 50K, else if the relationship status is different, the tree moves to node 2. 
# 3. We get 6 terminal nodes (leaf). 
# 4. Similarly, you can understand the splitting at other nodes.


# use the predict function to make predictions
prediction_train <- predict(train.tree, newdata=train, type="class")

# use the predict function to make predictions
prediction_test <- predict(train.tree, newdata=test, type="class")



# Analyse results through confusion matrix in caret package
library(caret)
confusionMatrix(prediction_train, factor(train$Income.Group))

# create a data frame of final prediction
solution_frame <- data.frame(ID=test$ID, Income.Group=prediction_test)

# write the solution file
write.csv(solution_frame, file="final_solution.csv")

library(randomForest)
model.rf<-train(Income.Group ~., data = train, 
                method="rf", 
                ntree=100)
result=predict(model.rf, newdata=test)

library(e1071)
confusionMatrix(result, train$Income.Group)




