##############################
## set a working directory####
setwd("C:/Users/Admin/Desktop")
getwd()

## ----
# Remember you need to set working directory to the folder which contains this data
ld=read.csv("loan.csv",stringsAsFactors = FALSE)
library(dplyr)
glimpse(ld)


## clean a data from null values and create dummy variables----

ld=ld %>%
  mutate(Interest.Rate=as.numeric(gsub("%","",Interest.Rate)) ,
         Debt.To.Income.Ratio=as.numeric(gsub("%","",Debt.To.Income.Ratio)) ,
         Open.CREDIT.Lines=as.numeric(Open.CREDIT.Lines) , 
         Amount.Requested=as.numeric(Amount.Requested) ,
         Amount.Funded.By.Investors=as.numeric(Amount.Funded.By.Investors),
         Revolving.CREDIT.Balance=as.numeric(Revolving.CREDIT.Balance)
         )
glimpse(ld)
  
## ------------------------------------------------------------------------

ld = ld %>%
  select(-Amount.Funded.By.Investors)


## ------------------------------------------------------------------------

ld= ld %>%
  mutate(f1=as.numeric(substr(FICO.Range,1,3)),
         f2=as.numeric(substr(FICO.Range,5,7)),
         fico=0.5*(f1+f2)
         ) %>%
  select(-FICO.Range,-f1,-f2)
glimpse(ld)


## ----
ld=ld %>%
  mutate(el=ifelse(substr(Employment.Length,1,2)=="10",10,Employment.Length),
         el=ifelse(substr(Employment.Length,1,1)=="<",0,el),
         el=gsub("years","",el),
         el=gsub("year","",el),
         el=as.numeric(el)
         ) %>%
  select(-Employment.Length) %>%
  na.omit()
  
# You must have noticed that we have also removed obs with missing values. 
# You could have imputed them if you want

## ------------------------------------------------------------------------
table(ld$Home.Ownership)

## ------------------------------------------------------------------------

ld=ld %>%
  mutate(HW_RENT=as.numeric(Home.Ownership=="RENT"),
         HW_MORT=as.numeric(Home.Ownership=="MORTGAGE"),
         HW_OWN=as.numeric(Home.Ownership=="OWN")) %>%
  select(-Home.Ownership)

## ------------------------------------------------------------------------
table(ld$Loan.Purpose)

## ------------------------------------------------------------------------
ld=ld %>%
  mutate(LP_cc=as.numeric(Loan.Purpose=="credit_card"),
         LP_dc=as.numeric(Loan.Purpose=="debt_consolidation"),
         LP_other=as.numeric(Loan.Purpose=="other")
         ) %>%
  select(-Loan.Purpose)

## ------------------------------------------------------------------------
table(ld$Loan.Length)

ld=ld %>%
  mutate(LL_36=as.numeric(Loan.Length=="36 months")) %>%
  select(-Loan.Length)

table(ld$State)

## ------------------------------------------------------------------------
ld= ld %>%
  select(-State)


## dividing a dataset in three parts train, validation and test.

set.seed(2)
s=sample(1:nrow(ld),0.7*nrow(ld))
ld_trainval=ld[s,]
ld_test=ld[-s,]

s1=sample(1:nrow(ld_trainval),0.7*nrow(ld_trainval))
ld_train=ld_trainval[s1,]
ld_val=ld_trainval[-s1,]


## ------------------------------------------------------------------------
glimpse(ld_train)

## ------------------------------------------------------------------------

fit=lm(Interest.Rate~. -ID,data=ld_train)


## ----
library(car)
vif(fit)

## ------------------------------------------------------------------------
fit=lm(Interest.Rate~. -ID - HW_MORT,data=ld_train)
vif(fit)

## ------------------------------------------------------------------------
summary(fit)

## ------------------------------------------------------------------------
fit=lm(Interest.Rate~. -ID - HW_MORT-Debt.To.Income.Ratio,data=ld_train)
summary(fit)

## ------------------------------------------------------------------------
fit=lm(Interest.Rate~. -ID - HW_MORT-Debt.To.Income.Ratio-el,data=ld_train)
summary(fit)
fit=lm(Interest.Rate~. -ID - HW_MORT-Debt.To.Income.Ratio  -el- LP_other,data=ld_train)
summary(fit)
fit=lm(Interest.Rate~. -ID - HW_MORT-Debt.To.Income.Ratio 
       - LP_other -el -Revolving.CREDIT.Balance ,data=ld_train)
summary(fit)
fit=lm(Interest.Rate~. -ID - HW_MORT-Debt.To.Income.Ratio 
       - LP_other -el -Revolving.CREDIT.Balance - HW_OWN,data=ld_train)
summary(fit)

fit=lm(Interest.Rate~. -ID - HW_MORT-Debt.To.Income.Ratio 
       - LP_other -el -Revolving.CREDIT.Balance - HW_OWN- HW_RENT,data=ld_train)
summary(fit)

## ------------------------------------------------------------------------
fit_train=fit

## ------------------------------------------------------------------------
library(ggplot2)
ld_train %>%
  mutate(pred_IR=predict(fit,newdata=ld_train)) %>%
  ggplot(aes(x=Interest.Rate,y=pred_IR))+geom_point(alpha=0.6)

## ------------------------------------------------------------------------
fit_val=lm(Interest.Rate~. -ID,data=ld_val)
vif(fit_val)
fit_val=lm(Interest.Rate~. -ID -HW_MORT,data=ld_val)
vif(fit_val)
summary(fit_val)
fit_val=lm(Interest.Rate~. -ID -HW_MORT-Debt.To.Income.Ratio,data=ld_val)
summary(fit_val)
fit_val=lm(Interest.Rate~. -ID -HW_MORT-Debt.To.Income.Ratio-Monthly.Income,data=ld_val)
summary(fit_val)
fit_val=lm(Interest.Rate~. -ID -HW_MORT-Debt.To.Income.Ratio-Monthly.Income-LP_other,data=ld_val)
summary(fit_val)
fit_val=lm(Interest.Rate~. -ID -HW_MORT-Debt.To.Income.Ratio-
             Monthly.Income-LP_other-Revolving.CREDIT.Balance,data=ld_val)
summary(fit_val)
fit_val=lm(Interest.Rate~. -ID -HW_MORT-Debt.To.Income.Ratio-
             Monthly.Income-LP_other-Revolving.CREDIT.Balance-el,data=ld_val)
summary(fit_val)

## ------------------------------------------------------------------------
summary(fit_train)
summary(fit_val)

## ------------------------------------------------------------------------
train_vars=names(fit_train$coefficients)
val_vars=names(fit_val$coefficients)

train_vars[train_vars %in% val_vars][-1]

## ------------------------------------------------------------------------
paste(train_vars[train_vars %in% val_vars][-1],collapse="+")
fit_final=lm(Interest.Rate~Amount.Requested+Open.CREDIT.Lines+
               Inquiries.in.the.Last.6.Months+fico+LP_cc+LP_dc+LL_36,data=ld_train)
summary(fit_final)

## ------------------------------------------------------------------------
model_string=paste(fit_final$coefficients,names(fit_final$coefficients),sep="*",collapse = " + ")
strwrap(sub("*(Intercept)","",gsub("+ -","- ",model_string,fixed=TRUE)))

## ------------------------------------------------------------------------
plot(fit_final,which=1)

## ------------------------------------------------------------------------
plot(fit_final,which=2)

## ------------------------------------------------------------------------
df=data.frame(res=fit_final$residual)
ggplot(df,aes(x=res))+geom_density(color="red")+
  stat_function(fun=dnorm ,aes(x=res),color="green")
shapiro.test(fit$residuals)

## ------------------------------------------------------------------------
plot(fit_final,which=3)

## ------------------------------------------------------------------------
plot(fit_final,which=4)

## ------------------------------------------------------------------------
#rmse for train
  mean((ld_train$Interest.Rate-predict(fit_final,newdata=ld_train))**2) %>%
  sqrt()
#rmse for test
  mean((ld_test$Interest.Rate-predict(fit_final,newdata=ld_test))**2) %>%
  sqrt()
  

## ----
# normal scatter plot
ld_temp=ld_train[ld_train$Monthly.Income<25000,]
ggplot(ld_temp,aes(x=fico,y=Interest.Rate))+geom_point()+ggtitle("Normal Scatter Plot")

## ------------------------------------------------------------------------
# Scatter Plot with density mapped
ggplot(ld_temp,aes(x=fico,y=Interest.Rate))+geom_point(alpha=0.4,size=5)

## ------------------------------------------------------------------------
#scatter plot with hex binning or 2d binning , you'll need to install package "hexbin" for the same
ggplot(ld_temp,aes(x=fico,y=Interest.Rate))+stat_binhex()
ggplot(ld_temp,aes(x=fico,y=Interest.Rate))+stat_bin2d()

