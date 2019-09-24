install.packages("AppliedPredictiveModeling")

library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData <- data.frame(diagnosis,predictors)
train <- createDataPartition(diagnosis, p = .5, list = F)
training <- adData[train,]
testing <- adData[-train,]
#answer is list = F, and both dfs in adData assignment



# Make a plot of the outcome (CompressiveStrength) versus the index of the samples. 
# Color by each of the variables in the data set (you may find the cut2() function in 
# the Hmisc package useful for turning continuous covariates into factors). 
# What do you notice in these plots?
data(concrete)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
training$index <- 1:nrows(training)
qplot(index, CompressiveStrength, data = training)
qplot(index, CompressiveStrength, color = Age,data = training)
qplot(index, CompressiveStrength, color = FlyAsh,data = training)
qplot(index, CompressiveStrength, color = Age,data = training)
qplot(index, CompressiveStrength, color = FlyAsh,data = training)
qplot(index, CompressiveStrength, color = Water,data = training)
qplot(index, CompressiveStrength, color = Superplasticizier,data = training)
#a slight pattern emerges, CS goes down w/ index, no var explains it

#why log transform won't work for superplaticizier
qplot(Superplasticizer, data = training)
qplot(log(Superplasticizer), data = training)
qplot(log(Superplasticizer+1), data = training) #same skeness


# Find all the predictor variables in the training set that begin with IL. 
# Perform principal components on these variables with the preProcess() function 
# from the caret package. Calculate the number of principal components needed 
# to capture 80% of the variance. How many are there?
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

#7 pc account for 80% of the variance
library(dplyr)
il <- select(training,diagnosis,starts_with("IL"))
preProcess(il, method = "pca", thresh = 0.8)

#compare fit with pca and without pca
library(caret)
library(AppliedPredictiveModeling)
library(dplyr)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

il <- select(training,diagnosis,starts_with("IL"))
iltest <- select(testing, diagnosis, starts_with("IL"))

#build and test model using PCS
ilPCA <- preProcess(il, method ="pca", thresh = .8)
trainPC <- predict(ilPCA, il) #update test dataset using PCA
modelFit <- train(diagnosis ~ ., method = "glm", data = trainPC)

testPC <- predict(ilPCA,iltest) #update training set using parameters of PCA from traing

confusionMatrix(testing$diagnosis,predict(modelFit,testPC))   

modelFit <- train(diagnosis ~ ., method = "glm", data = il)
confusionMatrix(testing$diagnosis, predict(modelFit,testing))
