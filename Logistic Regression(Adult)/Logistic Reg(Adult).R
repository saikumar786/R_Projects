#We will be working with the UCI adult dataset. We will be attempting to predict if people in the data
#set belong in a certain class by salary, either making <=50K or >50K per year.

#Get the data
adult <- read.csv('adult_sal.csv')

#head of adult
head(adult)

#Notice that index col is repeated. So, remove that col
library(dplyr)
adult <- select(adult, -X)

#head
head(adult)

#Structure of adult
str(adult)

#Summary of adult
summary(adult)

#Checking for nulls
is.na(adult)

#############################Data Cleaning#########################################
#type_employer col has 9 levels
table(adult$type_employer)
#We can clearly see that there are 1836 null values
#Now, combine the 2 smallest groups(Never-worked & Without-pay)
unemployed <- function(job){
  job <- as.character(job)
  if(job=='Never-worked' | job=='Without-pay'){
    return('Unemploy')
    }else{
      return(job)
    }
}
adult$type_employer <- sapply(adult$type_employer, unemployed)

table(adult$type_employer)
#Now, combine State & Local gov jobs into SL-Gov jobs and
#combine selfemployed to self-emp
group_emp <- function(job){
  if(job=='Local-gov' | job=='State-gov'){
    return('SL-gov')
  }else if(job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return('self-emp')
  }else{
    return(job)
  }
}
#Applying function
adult$type_employer <- sapply(adult$type_employer, group_emp)

#Check again
table(adult$type_employer)
#Now, it looks fair. So, go for another col

#Checking Marital col which has 7 levels
table(adult$marital)
#Group them into 3 groups only
#1.Married 2.Not-Married 3.Never-Married

group_marital <- function(marital){
  marital <- as.character(marital)
  #Not-Married
  if(marital=='Separated' | marital=='Widowed' | marital=='Divorced'){
    return('Not-Married')
  }else if(marital=='Never-married'){
    return(marital)
  }else{
    return('Married')
  }
}
#Apply this group_marital on Marital column
adult$marital <- sapply(adult$marital, group_marital)
table(adult$marital)


#Check the country col and reduce them as we wish(May be continentally or some other way)
table(adult$country)
#Check the levels of country
levels(adult$country)
#There are 42 countries
#Categorising them by Continent

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(country){
  if(country %in% Asia){
    return('Asia')
  }else if(country %in% North.America){
    return('North.America')
  }else if(country %in% Europe){
    return('Europe')
  }else if(country %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')
  }
}
#Apply group_country function on adult$country col
adult$country <- sapply(adult$country, group_country)
table(adult$country)

#Now, check the structure of the adult df
str(adult)

#So, convert type_employer, marital, country to factor
adult$type_employer <- factor(adult$type_employer)
adult$country <- factor(adult$country)
adult$marital <- factor(adult$marital)

#Structure of adult
str(adult)

#Check for Education col and reduce them too
table(adult$education)
group_edu <- function(edu_cls){
  edu_cls < as.character(edu_cls)
  if(edu_cls=='11th' | edu_cls=='12th'){
    return('Jr.clg')
  }else if(edu_cls=='Assoc-acdm' | edu_cls=='Assoc-voc'){
    return('Assoc-AV')
  }else if(edu_cls=='Bachelors' | edu_cls=='Masters' | edu_cls=='Doctorate'){
    return('Bachelors to Doctorate')
  }else if(edu_cls=='HS-grad' | edu_cls=='Some-college'){
    return('HSGradSomeclg')
  }else{
    return('Pre_scl -10th')
  }
}
#apply
adult$education <- sapply(adult$education, group_edu)
table(adult$education)

#Now, occupation col
table(adult$occupation)
#can do this as well. For now not working on it

#---------------------------Missing Data---------------------------------#
#Install Amelia library
#install.packages('Amelia')
library(Amelia)
adult[adult == '?'] <- NA
table(adult$type_employer)
#Now, we see 0 for ? values before 1836

#Refactor cols which had ? previously
adult$type_employer <- factor(adult$type_employer)
adult$country <- factor(adult$country)
adult$marital <- factor(adult$marital)
adult$occupation <- factor(adult$occupation)

#Now, use missmap(adult) which is a function from Ameila package
missmap(adult)#jst like heatmap which shows nulls
missmap(adult, y.at=c(1), y.labels = c(''), col=c('yellow', 'black'))

#Using na.omit() to omit NA data from the adult
adult <- na.omit(adult)
#str(adult)
#Use missmap to check again
missmap(adult, y.at=c(1), y.labels= c(''), col=c('yellow', 'black'))

#---------------------EDA-----------------------------#
library(ggplot2)
library(dplyr)

#Plotting Univariate(age)
ggplot(adult, aes(age)) + geom_histogram(aes(fill=income), color='black',binwidth=1) + theme_bw()

#Histogram of hours worked per week
ggplot(adult, aes(hr_per_week)) + geom_histogram() + theme_bw()

#Rename country col to region col to better reflect factor levels
names(adult)[names(adult)=="country"] <- "region"

#Barplot of region with fill color by income class
ggplot(adult, aes(region))+ geom_bar(aes(fill=income), color='black') + theme_bw() + theme(axis.text.x = element_text(angle = 90 , hjust=1))

#@@@@@@@@@@@@@@@ Building a Model @@@@@@@@@@@@@@@@@
#Logistic Regression

#Train Test Split
library(caTools)#For splitting

#Set a random seed
set.seed(101)

#Split up the sample
sample <- sample.split(adult$income, SplitRatio = 0.7)

#Training Data
train_data <- subset(adult, sample == T)

#Test data
test_data <- subset(adult, sample == FALSE)

#Training Model
#glm - package for Logistic Regression
help(glm)
model <- glm(income ~ ., family = binomial(logit), data=train_data)

#Check the summary of the model
summary(model)

#In R, we have a function called step() which iteratively tries to remove predictor variables from the model in an attempt to delete vars that aren't significant in fitting model
#It uses AIC value for removing
new.step.model <- step(model)

summary(new.step.model)

#Predictions
test_data$predicted.income <- predict(model, newdata = test_data, type = 'response')
table(test_data$income, test_data$predicted.income > 0.5)


#Accuracy
print('Accuracy:')
(6381+1422)/(6381+539+873+1422)

#Recall
6381/(6381+539)

#Precision
6381/(6381+873)

-------------------------0------------------------------
