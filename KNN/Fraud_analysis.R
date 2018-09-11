
# Load required libraries
library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(pROC)
library(glmnet)
library(caret)
library(Rtsne)
library(xgboost)
library(doMC)

# Load data
data <- fread("data/creditcard.csv")

head(data)
apply(data, 2, function(x) sum(is.na(x)))
common_theme <- theme(plot.title = element_text(hjust = 0.5, face = "bold"))
p <- ggplot(data, aes(x = Class)) + geom_bar() + ggtitle("Number of class labels") + common_theme
print(p)
summary(data)
p <- ggplot(data, aes(x = Class, y = Amount)) + geom_boxplot() + ggtitle("Distribution of transaction amount by class") + common_theme
print(p)


data %>% group_by(Class) %>% summarise(mean(Amount), median(Amount))

data$Class <- as.numeric(data$Class)
corr_plot <- corrplot(cor(data[,-c("Time")]), method = "circle", type = "upper")
normalize <- function(x){
  return((x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE))
}
data$Amount <- normalize(data$Amount)
# Use 10% of data to compute t-SNE
tsne_subset <- 1:as.integer(0.1*nrow(data))
tsne <- Rtsne(data[tsne_subset,-c("Class", "Time")], perplexity = 20, theta = 0.5, pca = F, verbose = T, max_iter = 500, check_duplicates = F)

classes <- as.factor(data$Class[tsne_subset])
tsne_mat <- as.data.frame(tsne$Y)
ggplot(tsne_mat, aes(x = V1, y = V2)) + geom_point(aes(color = classes)) + theme_minimal() + common_theme + ggtitle("t-SNE visualisation of transactions") + scale_color_manual(values = c("#E69F00", "#56B4E9"))

# Set random seed for reproducibility
set.seed(42)
# Transform "Class" to factor to perform classification and rename levels to predict class probabilities (need to be valid R variable names)
data$Class <- as.numeric(data$Class)
#data$Class <- revalue(data$Class, c("0"="false", "1"="true"))
#data$Class <- factor(data$Class, levels(data$Class)[c(2, 1)])
# Create training and testing set with stratification (i.e. preserving the proportions of false/true values from the "Class" column)
train_index <- createDataPartition(data$Class, times = 1, p = 0.8, list = F)
X_train <- data[train_index]
X_test <- data[!train_index]
y_train <- data$Class[train_index]
y_test <- data$Class[-train_index]
# Parallel processing for faster training
registerDoMC(cores = 8)
# Use 10-fold cross-validation
ctrl <- trainControl(method = "cv",
                     number = 10,
                     verboseIter = T,
                     classProbs = T,
                     sampling = "smote",
                     summaryFunction = twoClassSummary,
                     savePredictions = T)
log_mod <- glm(Class ~ ., family = "binomial", data = X_train)
summary(log_mod)
# Use a threshold of 0.5 to transform predictions to binary
conf_mat <- confusionMatrix(y_test, as.numeric(predict(log_mod, X_test, type = "response") > 0.5))
print(conf_mat)
fourfoldplot(conf_mat$table)
conf_mat2 <- confusionMatrix(y_test, as.numeric(predict(log_mod, X_test, type = "response") > 0.999))
print(conf_mat2)
roc_logmod <- roc(y_test, as.numeric(predict(log_mod, X_test, type = "response")))
plot(roc_logmod, main = paste0("AUC: ", round(pROC::auc(roc_logmod), 3)))
# Train a Random Forest classifier, maximising recall (sensitivity)
X_train_rf <- X_train
X_train_rf$Class <- as.factor(X_train_rf$Class)
levels(X_train_rf$Class) <- make.names(c(0, 1))
model_rf_smote <- train(Class ~ ., data = X_train_rf, method = "rf", trControl = ctrl, verbose = T, metric = "ROC")

model_rf_smote

preds <- predict(model_rf_smote, X_test, type = "prob")
conf_mat_rf <- confusionMatrix(as.numeric(preds$X1 > 0.5), y_test)
print(conf_mat_rf)

roc_data <- roc(y_test, predict(model_rf_smote, X_test, type = "prob")$X1)
plot(roc_data, main = paste0("AUC: ", round(pROC::auc(roc_data), 3)))

plot(varImp(model_rf_smote))
     
     dtrain_X <- xgb.DMatrix(data = as.matrix(X_train[,-c("Class")]), label = as.numeric(X_train$Class))
     dtest_X <- xgb.DMatrix(data = as.matrix(X_test[,-c("Class")]), label = as.numeric(X_test$Class))
     xgb <- xgboost(data = dtrain_X, nrounds = 100, gamma = 0.1, max_depth = 10, objective = "binary:logistic", nthread = 7)
     preds_xgb <- predict(xgb, dtest_X)
     confusionMatrix(as.numeric(preds_xgb > 0.5), y_test)
     
     roc_xgb <- roc(y_test, preds_xgb)
     plot(roc_xgb, main = paste0("AUC: ", round(pROC::auc(roc_xgb), 3)))
     
