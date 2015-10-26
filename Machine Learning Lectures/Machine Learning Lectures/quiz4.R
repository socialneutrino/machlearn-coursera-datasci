##Question 1

library(ElemStatLearn)
library(caret)
data(vowel.train)
vowel.train$y <- as.factor(vowel.train$y)
data(vowel.test)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)

modRF <- train(y ~ ., data=vowel.train, method="rf")
modGBM <- train(y ~ ., data=vowel.train, method="gbm")

modpredict <- data.frame(testdata=vowel.test$y,
                            RF=predict(modRF, vowel.test),
                            GBM=predict(modGBM, vowel.test))

x <- vector()
for (i in seq_along(modpredict$testdata)) {
  x[i] <- modpredict$RF[i] == modpredict$GBM[i]
}

modpredict <- cbind(modpredict, agree=x)

xi <- vector()
for (i in seq_along(modpredict$testdata)) {
  xi[i] <- (modpredict$testdata[i] == modpredict$GBM[i]) && modpredict$agree[i]
}

modpredict <- cbind(modpredict, agreepredict=xi)

confusionMatrix(vowel.test$y,predict(modRF, vowel.test))
confusionMatrix(vowel.test$y,predict(modGBM, vowel.test))

sum(modpredict$agreepredict)/sum(modpredict$agree)

results<- data.frame(RF=confusionMatrix(vowel.test$y,predict(modRF, vowel.test))$overall[1],
GBM=confusionMatrix(vowel.test$y,predict(modGBM, vowel.test))$overall[1],
Agree=sum(modpredict$agreepredict)/sum(modpredict$agree))


##Question 2

library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)

modRF <- train(training$diagnosis~., data=training, method="rf")
modGBM <- train(training$diagnosis~., data=training, method="gbm")
modLDA <- train(training$diagnosis~., data=training, method="lda")

predict.RF <- predict(modRF, training)
predict.GBM <- predict(modGBM, training)
predict.LDA <- predict(modLDA, training)

comb.training <- data.frame(diagnosis=training$diagnosis,predict.RF,predict.GBM,predict.LDA)
combRF <- train(diagnosis~., data=comb.training, method="rf")

test.RF <- predict(modRF, testing)
test.GBM <- predict(modGBM, testing)
test.LDA <- predict(modLDA, testing)

comb.testing <- data.frame(predict.RF=test.RF,predict.GBM=test.GBM,predict.LDA=test.LDA)
predict.comb <- predict(combRF, comb.testing)

test.accuracy <- data.frame(RF=confusionMatrix(testing$diagnosis, test.RF)$overall[1],
                            GBM=confusionMatrix(testing$diagnosis, test.GBM)$overall[1],
                            LDA=confusionMatrix(testing$diagnosis, test.LDA)$overall[1],
                            stacked=confusionMatrix(testing$diagnosis, predict.comb)$overall[1])
