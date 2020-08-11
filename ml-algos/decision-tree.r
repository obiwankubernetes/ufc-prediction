#load packages
library(C50)
library(gmodels)

#randomizing data for training and test sets
set.seed(12345)
fight_rand <- fight_norm[order(runif(6230)), ]

#split data into training and test
fight_train <- fight_rand[1:4984, ]
fight_test  <- fight_rand[4985:6230, ]

#1st model 
trees <- C5.0(fight_train[-1], fight_train$result)

#displaying model
trees

#creating predictions on test data
fight_pred <- predict(trees, fight_test)

#cross table of predicted versus actual classes
CrossTable(fight_test$result, fight_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#2nd model with boosting
boost10 <- C5.0(fight_train[-1], fight_train$result,
                       trials = 10)
#viewing boosted model
boost10
summary(boost10)

#creating prediciton on test data
fight_boost_pred10 <- predict(boost10, fight_test)
CrossTable(fight_test$result, fight_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
