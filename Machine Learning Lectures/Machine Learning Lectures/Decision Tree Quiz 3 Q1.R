]#

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
set.seed(125)

#what is wrong with my partitioning?
#inTrain <- createDataPartition(y=segmentationOriginal$Case, times=1, list=FALSE)

train.1 <- segmentationOriginal[segmentationOriginal$Case=="Train",]
test.1 <- segmentationOriginal[segmentationOriginal$Case=="Test",]
tr <- train(Class ~ ., method = "rpart", data = train.1)

fancyRpartPlot(tr$finalModel)
