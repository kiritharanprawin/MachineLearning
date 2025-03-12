library(mlbench)
data("PimaIndiansDiabetes2")
#just to not use the long data name
diabetes<-PimaIndiansDiabetes2

#The caret package contains functions to streamline the model training process for complex
#regression and classification problems

library(caret)

indxTrain <- createDataPartition(y = diabetes$diabetes,p = 0.8,list = FALSE)
train <- diabetes[indxTrain,] # 80% training data
test <- diabetes[-indxTrain,] # 20% testing data

#to test distribution of classes in both training and testing data sets
prop.table(table(train$diabetes)) * 100
prop.table(table(test$diabetes)) * 100

caret_glm_mod = train(
  form = diabetes ~ .,
  data = train,
  #trControl = trainControl(method = "cv", number = 5),
  trControl = trainControl(method = "cv", number = 5),
  method = "glm",
  family = "binomial",
  na.action = na.exclude
)



#To check the final model we can use the train() object parameter or use summary() function.
caret_glm_mod$finalModel
#or
summary(caret_glm_mod)