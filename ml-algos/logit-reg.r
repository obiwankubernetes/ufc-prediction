#load packages
library(lattice)
library(ggplot2)
library(caret)
library(e1071)

#1st logit model
logit.model <- glm(result ~ . , data = fight_norm, family = "binomial" )
summary(logit.model)

#removing insignificant variables to tune model
fight$Str..Acc..x <- NULL
fight$Str..Acc..y <- NULL
fight$TD.Acc..x <- NULL
fight$TD.Acc..y <- NULL

#2nd regression model with insignificant variables removed
logit.model2 <- glm(result ~ ., family ="binomial", data = fight_norm)
summary(logit.model2)
pred2 <- predict(logit.model2, newdata = fight_norm, type = "response")
predbinary2 <- ifelse(pred2>0.5,"win","loss")
predbinary2
predbinary2 <- as.factor(predbinary2)
confusionMatrix(predbinary2, fight$result, positive="win")