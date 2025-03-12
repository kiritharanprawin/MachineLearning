library(mlbench)
data("PimaIndiansDiabetes2")
diabetes<-na.omit(PimaIndiansDiabetes2)
logit <- glm(diabetes~glucose,family = binomial,data = diabetes)
#returns the estimate, standard errors, z-score, and p-values on each of the coefficients
summary(logit)

library(tidyverse)
train2 <-mutate(diabetes,prob = ifelse(diabetes =="pos",1,0))

ggplot(train2,aes(glucose,prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm",method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model",
    x = "Plasma Glucose Concentration",
    y = "Probability of being diabete-positive"
  )

#Predictions can be easily made using the function predict(). 
#Use the option type = “response” to directly obtain the probabilities
newdata <- data.frame(glucose = c(20,180))
probabilities <- predict(logit,newdata,type = "response")
probabilities

#convert the probability response variable to the categorical one
predicted.classes <- ifelse(probabilities > 0.5, "pos","neg")
predicted.classes

#extract fitted values for the diabetes dataset use fitted.values
diabetes.predicted <- logit$fitted.values
diabetes.classes <- ifelse(diabetes.predicted > 0.5,"pos","neg")

#Check how many observation were correctly classified
table(diabetes.classes,diabetes$diabetes)

#Calculate proportion of correctly classified observation
mean(diabetes.classes == diabetes$diabetes)

