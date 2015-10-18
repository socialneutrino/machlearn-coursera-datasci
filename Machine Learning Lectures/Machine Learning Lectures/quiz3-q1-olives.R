library(pgmm)
data(olive)
olive = olive[,-1]
inTrain <- createDataPartition(y=olive$Area, p=0.7, list=FALSE)
training <- olive[inTrain,]
testing <- olive[-inTrain,]
modFit <- train(Area ~ .,method="rpart",data=training)
print(modFit$finalModel)
newdata = as.data.frame(t(colMeans(olive)))
predict(modFit,newdata)