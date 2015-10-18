library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
#Coronary Heart Disease (chd) as the outcome and age at onset, 
#current alcohol consumption, obesity levels, cumulative tabacco, 
#type-A behavior, and low density lipoprotein cholesterol as predictors
logModel <- train(chd ~ age +alcohol + obesity + tobacco + typea + ldl,
                  method="glm", family="binomial", data=trainSA)

##missclassification function
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

predTrain <- predict(logModel)
missClass(trainSA$chd, predTrain)
predTest <- predict(logModel, newdata=testSA)
missClass(testSA$chd, predTest)