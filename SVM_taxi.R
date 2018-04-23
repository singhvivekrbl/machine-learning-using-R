# enable parallel processing as the process is computationally intensive.
library(doMC)
registerDoMC(cores = 4)
# read input driver attribute files. 

setwd("/Users/sundar/data/input/NYCDSA/attr")
allfiles <- dir(pattern="*.RData")

data <- attributes.raw <- NULL
attributes.raw <- data.frame()

for(i in 1:length(allfiles)){
  data <- NULL 
  load(allfiles[i])
  if (length(attributes.raw) == 0 ){
    attributes.raw <- data 
  }
  else {
    attributes.raw <- rbind(attributes.raw,data)
  }            
}

# convert "driver" field as the class factor.
attributes.raw$driver <- as.factor(attributes.raw$driver)

# check for near zero variance variables.
nzv <- nearZeroVar(driver.attributes[,-26])
head(nzv)  
## integer(0)
# No near zero variance attributes.
#Find if there are NA Values in the input data and impute: 

impute.Attribute <- function(impute.input){ 
  nullcount  <-   sum(I(is.na(impute.input)))
  if (nullcount > 0){
    imputed <<- complete(mice(impute.input,m=5,,print=FALSE))
  }
  else {
    imputed <<- impute.input          
  }
}

impute.Attribute(driver.attributes)
# Function to partition test and training datasets

prep.for.model <- function(prep.input) { 
  inTest <- createDataPartition(y = prep.input$driver, p = 0.2, list = FALSE)
  imputed$driver <<- as.factor(prep.input$driver)
  testing <<- prep.input[inTest,]
  training <<- prep.input[-inTest,]
}

set.seed(1)
prep.for.model(imputed)
#setup control file 

cvCtrl= trainControl(method="repeatedcv",repeats=5,
                     verbose=F,
                     classProbs=T)
set.seed(2)
svmTune <- train(driver ~., data=training,
                 method="svmRadial",
                 tuneLength=10,
                 preProc=c("center","scale"),
                 metric="Accuracy",
                 trControl=cvCtrl)
svmTune  # display the tuning details
ggplot(svmTune) + scale_x_log10() + theme_bw() # plot the ROC curve 

svmTune$finalModel # plot the ROC curve 
##Predict classes for the testing dataset and evaluate performance. 

svmPred <- predict(svmTune, newdata=testing)
confusionMatrix(svmPred,testing$driver)

