library(mlbench)
data("PimaIndiansDiabetes2")
#just to not use the long data name
diabetes<-PimaIndiansDiabetes2

#The caret package contains functions to streamline the model training process for complex
#regression and classification problems

library(caret)

# split data into training and testing set good practice 80%-20% or 70%-30%
#random sampling
indexes<-sample(1:nrow(diabetes), 4/5*nrow(diabetes))
train<-diabetes[indexes,] # Select 80% of data for training
test<-diabetes[-indexes,] # Use the remaining 20% for testing

#to test distribution of classes in both training and testing datasets
prop.table(table(train$diabetes)) * 100
prop.table(table(test$diabetes)) * 100

#Dataset can also be split using some advanced method from caret package:
indxTrain <- createDataPartition(y = diabetes$diabetes,p = 0.8,list = FALSE)
train <- diabetes[indxTrain,] # 80% training data
test <- diabetes[-indxTrain,] # 20% testing data

#to test distribution of classes in both training and testing datasets
prop.table(table(train$diabetes)) * 100
prop.table(table(test$diabetes)) * 100
