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


##Question 2 - stacking predictions into RF

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

##Question 3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
library(lars)
lasso.fit <- lars(as.matrix(training[,-9]), training$CompressiveStrength, type="lasso", trace=TRUE)
lasso.enet <- enet(as.matrix(training[,-9]), training$CompressiveStrength, lambda = 0)

##Question 4 - forecasts and time series

download.file(url="https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv",
              destfile="gaData.csv")
library(lubridate)  

# For year() function below
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

library(forecast)
forecast.fit <- bats(tstrain)
forecast.test <- predict(forecast.fit, ts(testing$visitsTumblr))
fcast <- forecast(fit,h=235)
bounds <- data.frame(lower=fcast$lower[,2], upper=fcast$upper[,2], test=ts(testing$visitsTumblr))
bounds$inRange <- (bounds$test > bounds$lower) * (bounds$test < bounds$upper)

# Question 5

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)
fit.svm <- svm(CompressiveStrength ~., data=training)
predict.svm <- predict(fit.svm, testing)
svm.residuals <- (predict.svm - testing$CompressiveStrength)
rmse <- function(error)
{
  sqrt(mean(error^2))
}
rmse(svm.residuals)