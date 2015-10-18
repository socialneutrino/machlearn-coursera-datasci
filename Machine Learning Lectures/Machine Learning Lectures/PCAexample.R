library(caret)
> library(AppliedPredictiveModeling)
> set.seed(3433)
> data(AlzheimerDisease)
> adData = data.frame(diagnosis,predictors)
> inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
> training = adData[ inTrain,]
> testing = adData[-inTrain,]
> preProc <- preProcess(ILtrain, method="pca", thresh=0.8)
> preProc

Call:
  preProcess.default(x = ILtrain, method = "pca", thresh = 0.8)

Created from 251 samples and 12 variables
Pre-processing: principal component signal extraction, scaled, centered 

PCA needed 7 components to capture 80 percent of the variance
> ILtrain <- training[,58:69]
> 