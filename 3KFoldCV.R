# Data partitioning 
# K-fold crossvalidation with RandomForest and Support Vector Machine 
# Confusion Matrix

library(dplyr)
library(class)
library(e1071)
library(caret)
library(randomForest)
library(kernlab)


# Set working directory
setwd("C:/Users/Mini/Desktop/PhD/Neuroloska PhD/!GaitRiteFinal")

#Read data
Data<-read.csv("!PrepakovaniSVIfinal.csv")


#izbaci redne brojeve  i imena! osim ako ih nema
Data<-Data[,-c(1,ncol(Data))]


modelujRF<-function(inTest){
  
  training<-Data[-inTest,]
  test<-Data[inTest,]
  
  # random forest
  rfmodel<-randomForest(dijagnoza~.,data=training,importance=T)
  rftest<-predict(rfmodel,test)
  cmRF<-confusionMatrix(rftest,test$dijagnoza)

}

modelujSVM<-function(inTest){
  
  fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 3,
    ## repeated ten times
    repeats = 10)
  training<-Data[-inTest,]
  test<-Data[inTest,]
  
  
# rbf svm
rbfmodel<-train(dijagnoza~.,data=training,method="svmRadial",trControl=fitControl)
rbftest<-predict(rbfmodel,test)
cmRBF<-confusionMatrix(rbftest,test$dijagnoza)
}

#### stratified k-fold crossvalidation

# number of folds
k<-5

set.seed(582)

folds<-createFolds(Data$dijagnoza,k,returnTrain=F)

fitsRF<-lapply(folds,modelujRF)
fitsSVM<-lapply(folds,modelujSVM)

AccuracyRF<-c()
AccuracySVM<-c()

for (i in 1:k)
{
  AccuracyRF<-c(AccuracyRF,fitsRF[[i]]$overall[1])
  AccuracySVM<-c(AccuracySVM,fitsSVM[[i]]$overall[1])
}




