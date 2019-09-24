
library(ElemStatLearn)
library(randomForest)
library(caret)
data(vowel.train)
data(vowel.test)

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
set.seed(33833)
modFitRF <- train(y ~., data = vowel.train, method = "rf")
modFitG <- train(y ~., data = vowel.train, method = "gbm")

predictRF <- confusionMatrix(vowel.test$y, predict(modFitRF,vowel.test)) #.5909
predictG <- confusionMatrix(vowel.test$y, predict(modFitG,vowel.test)) #.53

agreement <- predict(modFitRF,vowel.test) == predict(modFitG, vowel.test)

vowel.test$prediction <- predict(modFitRF,vowel.test)
vowel.test.agree <- vowel.test[agreement,]
sum(vowel.test.agree$y == vowel.test.agree$prediction)/nrow(vowel.test.agree) #.62



##Q2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)

data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
modFitRF <- train(diagnosis ~., data = training, method = "rf")
modFitG <- train(diagnosis ~., data = training, method = "gbm")
modFitL <- train(diagnosis ~., data = training, method = "lda")

conMRF <-  confusionMatrix(testing$diagnosis, predict(modFitRF,testing)) #.79
conMG <-  confusionMatrix(testing$diagnosis, predict(modFitG,testing)) #.78
conML <-  confusionMatrix(testing$diagnosis, predict(modFitL,testing)) #.7683

pr1 <- predict(modFitRF,testing)
pr2 <-predict(modFitG,testing) 
pr3 <- predict(modFitL,testing)

preDF <- data.frame(pr1,pr2,pr3, diagnosis = testing$diagnosis)

comModFit <- train(diagnosis ~., method = "rf", data = predDF)
combPred <- predict(comModFit, predDF)

sum(combPred == testing$diagnosis)/nrow(testing) #.804878, better than RF and lda, same boosing

##Q3
set.seed(3523)
library(AppliedPredictiveModeling)
library(elasticnet)
library(caret)

data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

fit<- train(CompressiveStrength~., data=training, method = "lasso")

plot.enet(fit$finalModel)  ##NOT COURSE AGGREGATE, MUST BE FINE AGGREGATE?
plot(fit$finalModel, xvar="penalty", use.color=T) #CEMENENT!


#Q4
library(lubridate)
library(forecast)
setwd("~/Dropbox/machinelearning")
list.files()
dat <- read.csv("gaData.csv", stringsAsFactors = F)

training = dat[year(dat$date) < 2012,]

testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)
tstest <- ts(testing$visitsTumblr)


fit <- bats(tstrain)
ftest <- forecast(tstest, model = fit, h = 235)

lower <- as.data.frame(ftest$lower)
colnames(lower)[2] <- "lower" 
upper <- as.data.frame(ftest$upper)
colnames(upper)[2] <- "upper"

results <- data.frame(cbind(lower = lower$lower, 
                 upper = upper$upper, 
                 testing = testing$visitsTumblr))

results$ininterval <- (results$testing > results$lower & results$testing < results$upper)
sum(results$ininterval)/nrow((results)) # 0.9574468

#Q5
set.seed(3523)
library(AppliedPredictiveModeling)
library(e1071)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(352)
model <- svm(CompressiveStrength ~ ., data = training)
pred <- predict(model, testing)
sqrt(mean((testing$CompressiveStrength - pred)^2)) #6.715009
