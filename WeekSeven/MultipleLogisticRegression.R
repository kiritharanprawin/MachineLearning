library(mlbench)
data("PimaIndiansDiabetes2")
#just to not use the long data name
diabetes<-PimaIndiansDiabetes2
logit2 <-glm(diabetes~., family = binomial, data = diabetes)
summary(logit2)

#Calculate the percentage of correctly classified observation
probabilities <- predict(logit2, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
# Model accuracy
mean(predicted.classes == diabetes$diabetes)
