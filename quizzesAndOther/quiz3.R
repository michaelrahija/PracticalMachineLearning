library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(dplyr)

#Q2 The bias is larger and the variance is smaller. Under leave one out cross validation K is equal to the sample size.
#
inTrain = createDataPartition(segmentationOriginal$Class, p = .6, list =F)
training = segmentationOriginal[ inTrain,]
testing = segmentationOriginal[-inTrain,]
set.seed(125)
modFit <- train(Class ~ ., method = "rpart", data = training)

print(modFit$finalModel)
# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
# PS
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
# WS
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
# PS
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 
# not possible to know
# 
###Q3 
library(pgmm)
library(caret)
data(olive)
olive = olive[,-1]

newdata = as.data.frame(t(colMeans(olive)))

inTrain = createDataPartition(olive$Area, p = .6, list =F)
training = olive[ inTrain,]
testing = olive[-inTrain,]
modFit <- train(Area ~ ., method = "rpart", data = training)
print(modFit$finalModel)

predict(modFit,newdata)


#Q4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed("13234")
modFit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
                method = "glm", family = "binomial", data = trainSA)

trainResult <- predict(modFit, trainSA)
testResult <- predict(modFit,testSA)

missClass = function(values,prediction){
  
  sum(((prediction > 0.5)*1) != values)/length(values)
  
  }
missClass(trainSA$chd,trainResult)
missClass(testSA$chd,testResult)
#test .31
#train .27

#Q5
#
library(ElemStatLearn)
library(randomForest)
data(vowel.train)
data(vowel.test)

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
set.seed(33833)
modFitC <- train(y ~., data = vowel.train, method = "rf")

modFit <- randomForest(y ~., data = vowel.train)
arrange(importance(modFit))
#2,1,5,6,8