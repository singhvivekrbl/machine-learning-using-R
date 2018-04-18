setwd("C:/Users/Admin/Desktop")
getwd()
bt=read.csv("sample_data_test.csv",stringsAsFactors = F)
  
library(dplyr)
glimpse(bt)
#scaling
bt=bt %>%
  select(transaction_amount,transaction_location,dependents)
glimpse(bt)
## ------------------------------------------------------------------------
md=function(x){
  return((x-mean(x))/sd(x))
}

bt_std=bt %>%
  mutate(transaction_amount=md(transaction_amount),
         transaction_location=md(transaction_location),
         dependents=md(dependents))

wss=(nrow(bt_std)-1)*sum(apply(bt_std,2,var))
for(i in 2:15) wss[i]= sum(kmeans(bt_std,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")





require(vegan)
fit = cascadeKM(bt_std, 1, 10, iter = 100)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best = as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")

## ------------------------------------------------------------------------
fit = kmeans(bt_std,10) 
fit
summary(fit)
bt_std$cluster=fit$cluster
fit$centers


library(ggplot2)

#pair wise profiling plots
ggplot(bt_std,aes(transaction_amount,transaction_location,color=as.factor(cluster)))+geom_point()
ggplot(bt_std,aes(transaction_amount,dependents,color=as.factor(cluster)))+geom_point()
ggplot(bt_std,aes(transaction_location,dependents,color=as.factor(cluster)))+geom_point()
