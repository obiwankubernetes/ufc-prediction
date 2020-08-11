#Running a KNN Model

# loading libraries
library(class)
library(gmodels)


#randomize the data and set up the test and train data
set.seed(10000) 
train_data <- sample(1:nrow(fight_norm), round(.7*nrow(fight_norm)))
fight_train1 <- fight_norm[train_data, ]
fight_test1 <- fight_norm[-train_data, ]

#baseline KNN model for
knn_test <- knn(train = fight_train1[,-c(1,2)], test = fight_test1[,-c(1,2)], cl = fight_train1$resultloss, k = 3)
plot(knn_test)
# create confusion matrix
CrossTable(x = fight_test1$resultloss, y = knn_test, prop.chisq = FALSE)
confusionMatrix(as.factor(fight_test1$resultloss), as.factor(knn_test))

#Improving KNN models
knn_test <- knn(train = fight_train1[,-c(1,2)], test = fight_test1[,-c(1,2)], cl = fight_train1$resultloss, k = 5)
plot(knn_test)

# creating confusion matrix
CrossTable(x = fight_test1$resultloss, y = knn_test, prop.chisq = FALSE)
confusionMatrix(as.factor(fight_test1$resultloss), as.factor(knn_test))
