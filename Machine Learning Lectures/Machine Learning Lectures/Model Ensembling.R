library(ISLR)
library(caret)
library(gbm)
set.seed(3433)

inBuild <- createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
validation <- Wage[-inBuild,]; buildData <- Wage[inBuild,]
inTrain <- createDataPartition(y=buildData$wage,p=0.7, list=FALSE)
training <- buildData[inTrain,]; testing <- buildData[-inTrain,]

# train the data using both glm and random forest models
glm.fit <- train(wage ~.,method="glm",data=training)
rf.fit <- train(wage ~.,method="rf",data=training,
                trControl = trainControl(method="cv"),number=3)

# use the models to predict the results on the testing set
glm.pred.test <- predict(glm.fit,testing)
rf.pred.test <- predict(rf.fit,testing)

# combine the prediction results and the true results into new data frame
combinedTestData <- data.frame(glm.pred=glm.pred.test,
                               rf.pred = rf.pred.test,wage=testing$wage)

# run a Generalized Additive Model (gam) model on the combined test data
comb.fit <- train(wage ~.,method="gam",data=combinedTestData)

# use the resultant model to predict on the test set

comb.pred.test <- predict(comb.fit, combinedTestData)

# use the glm and rf models to predict results on the validation data set
glm.pred.val <- predict(glm.fit,validation)
rf.pred.val <- predict(rf.fit,validation)

# combine the results into data frame for the comb.fit
combinedValData <- data.frame(glm.pred=glm.pred.val,rf.pred=glm.pred.val)

# run the comb.fit on the combined validation data
comb.pred.val <- predict(comb.fit,combinedValData)

# tabulate the results - test data set RMSE Errors
rbind(test = c(glm = sqrt(sum((glm.pred.test-testing$wage)^2)),
               rf = sqrt(sum((rf.pred.test-testing$wage)^2)),
               combined = sqrt(sum((comb.pred.test-testing$wage)^2))),
      
      # validation data set RMSE Errors
      validation = c(sqrt(sum((glm.pred.val-validation$wage)^2)),
                     sqrt(sum((rf.pred.val-validation$wage)^2)),
                     sqrt(sum((comb.pred.val-validation$wage)^2))))

