#Download testing and training data
download.file(url="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile="pml-training.csv")
training <- read.csv("pml-training.csv")
download.file(url="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile="pml-testing.csv")
testing <- read.csv("pml-testing.csv")

library(caret)
numNA <- sapply(training, function(x) sum(is.na(x)))
covariates <- names(sumNA[sumNA==0])
train1 <- subset(training, select = covariates)
library(caret); library(kernlab); set.seed(1111)

inTrain <- createDataPartition(y=train1$classe, p=0.75, list=FALSE)
firstTrain <- train1[inTrain,]
firstTest <- train1[-inTrain,]
