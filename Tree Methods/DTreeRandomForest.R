#We get the data which is included in the ISLR library
#Call ISLR library
#install.packages("ISLR")
library(ISLR)

college_data <- read.csv(file = 'College_Data.csv', sep = ',')
#Head of the dataframe
head(college_data)

#Dimension of the data
dim(college_data)

#Structure of the data
str(college_data)

#Rename the column X to ""
names(college_data)
names(college_data)[1] <- "College"

#Missing Value Analysis
apply(college_data, 2, function(x){sum(is.na(x))})
#na.omit(college_data)

#----------------------------------------EDA--------------------------------------------------
#Scatterplot of Grad.Rate vs Room.Board colored by Private
library(ggplot2)

ggplot(college_data, aes(Room.Board, Grad.Rate)) + geom_point(aes(color=Private))

#Histogram of F.Undergrad(Full time Undergraduates) color by Private
ggplot(college_data, aes(F.Undergrad)) + geom_histogram(aes(fill=Private), color='Black', bins=40)

#Histogram of Grad.Rate color by Private
ggplot(college_data, aes(Grad.Rate)) + geom_histogram(aes(fill=Private), color = 'black', bins=40)
#From the above plot, we can clearly see that Graduation Rate crossed 100%. which is unusual.
#Make that observation's Grad.Rate to valid Grad.Rate of Max(100%)
subset(college_data, Grad.Rate>100)
college_data['Cazenovia College', 'Grad.Rate'] <- 100

#Make target var(Private) to numerical
private.col <- function(private_var){
  if(private_var == 'Yes'){
    private_var <- 1
  }else if(private_var == 'No'){
    private_var <- 0
  }else{
    private_var <- 0
  }
}

college_data <- sapply(college_data$Private, private.col)

#####Train Test Split########
library(caTools)

set.seed(201)

#Sample
sample <- sample.split(college_data$Private, SplitRatio = 0.75)
train <- subset(college_data, sample==TRUE)
test <- subset(college_data, sample==FALSE)

#Decision Tree
library(rpart)

tree <- rpart(Private ~ Apps + Grad.Rate + PhD + F.Undergrad + P.Undergrad + Room.Board, method = 'class', data = train)

#Use predict() to make predictions
tree.predictions <- predict(tree, test)

head(tree.predictions)

#Turning this df which has 1 column instead of 2 cols(Yes and No)
tree.predictions <- as.data.frame(tree.predictions)
#using a function
pred.yes.no <- function(x){
    if(x>=0.5){
      return('Yes')
    }else{
      return('No')
    }
}

tree.predictions$Private <- sapply(tree.predictions$Yes, pred.yes.no)

#Creating Confusion Matrix
table(tree.predictions$Private, test$Private)

#Use rpart.plot and prp() function to plot out the tree model we have built
library(rpart.plot)
prp(tree)

#######-Random Forest-##############
library(randomForest)

rf.model <- randomForest(Private ~ -c('College'), data = train, importance = TRUE)

#Confusion Matrix
rf.model$confusion

#Grabbing the feature importance
rf.model$importance

####Predictions of RandomForest model
rf.predictions <- predict(rf.model, test)

table(rf.predictions, test$Private)

#We can say that the RandomForest has performed better than the Decision Tree as it is an ensemble method








