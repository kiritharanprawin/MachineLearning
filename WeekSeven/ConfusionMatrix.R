library(mlbench)
data("PimaIndiansDiabetes2")
diabetes<-PimaIndiansDiabetes2

#The caret package contains functions to streamline the model training process for complex
#regression and classification problems

library(caret)

indxTrain <- createDataPartition(y = diabetes$diabetes,p = 0.8,list = FALSE)
train <- diabetes[indxTrain,] # 80% training data
test <- diabetes[-indxTrain,] # 20% testing data

prop.table(table(train$diabetes)) * 100
prop.table(table(test$diabetes)) * 100

caret_glm_mod = train(
  form = diabetes ~ .,
  data = train,
  trControl = trainControl(method = "cv", number = 5),
  method = "glm",
  family = "binomial",
)

caret_glm_mod$finalModel

predicted_test <-predict(caret_glm_mod,newdata = test)
confusionMatrix(predicted_test,test$diabetes,positive ="pos")

