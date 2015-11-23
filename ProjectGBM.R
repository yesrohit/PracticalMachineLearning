library(caret)

setwd('C:/Users/RohitMittal/Desktop/Practical Machine Learning')
data = read.csv("pml-training.csv")

dataNoNA=data
dataNoNA[dataNoNA==""]<-NA
dataNoNA=dataNoNA[,colSums(is.na(dataNoNA))==0]
CleanData=dataNoNA[,-c(1,3,4,5,6,7)]
ColsChosen=sapply(colnames(data), function(x) grep(x,colnames(CleanData)))

inTrain = createDataPartition(CleanData$classe, p=0.75,list=FALSE)
Training = CleanData[inTrain,]
Testing = CleanData[-inTrain,]

modelFit = train(CleanData$classe~.,method="gbm",data=CleanData, trControl = trainControl(method="cv",number=10))
confusionMatrix(predict(modelFit,CleanData),CleanData$classe)

TestData = read.csv("pml-testing.csv")
testClean = TestData[,(which(sapply(ColsChosen, function(x) length(x)>0)))]

confusionMatrix(predict(modelFit,testClean),testClean$classe)
TestingClass=predict(modelFit,testClean)


pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

