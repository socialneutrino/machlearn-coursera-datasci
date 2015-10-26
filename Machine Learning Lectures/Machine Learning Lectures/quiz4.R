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