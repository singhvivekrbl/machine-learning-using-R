############################
##loading required libraries
############################
library(caret)
library(class)
library(e1071)
library(ROCR)
##read the file
credit <- read.csv("creditcard.csv")
str(credit)
credit$Class <- factor(credit$Class)
set.seed(1998)
samp <- sample(1:nrow(credit), round(0.2*nrow(credit)))
credit <- credit[samp, ]
index <- createDataPartition(credit$Class, p = 0.75, list = F)
train <- credit[index, ]
test <- credit[-index, ]
knn1 <- knn(train = train[,-31], test = test[,-31], cl = train$Class, k = 5)
confusionMatrix(knn1, test$Class, positive = "1")
bayes <- naiveBayes(Class~., data = train, laplace = 1)
bayes$apriori
pred <- predict(bayes, test)
confusionMatrix(pred, test$Class, positive = "1")
rawpred <- predict(bayes, test, type = "raw")
ptest <- prediction(rawpred[,2], test$Class)
perf <- performance(ptest, "tpr", "fpr")
plot(perf, colorize = T)
performance(ptest, "auc")@y.values
