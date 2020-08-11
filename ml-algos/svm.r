#loading packages
library(kernlab)

#using train and test data from decision-tree.r

#creating 1st model
fight_classifier <- ksvm(result ~ ., data = fight_train,
                          kernel = "vanilladot")
fight_classifier

#creating predictions on testing dataset
fight_predictions <- predict(fight_classifier, fight_test)
head(fight_predictions)

#creating table with predictions vs results
table(fight_predictions, fight_test$result)

#creating table for agreement vs. non-agreement
agreement <- fight_predictions == fight_test$result
table(agreement)
prop.table(table(agreement))

#2nd model improving SVM model performance with rbfdot
fight_classifier_rbf <- ksvm(result ~ ., data = fight_train, kernel = "rbfdot")

#creating predictions
fight_predictions_rbf <- predict(fight_classifier_rbf, fight_test)

#creating agreement table
agreement_rbf <- fight_predictions_rbf == fight_test$result
table(agreement_rbf)
prop.table(table(agreement_rbf))