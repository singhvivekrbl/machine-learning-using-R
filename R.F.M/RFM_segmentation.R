#######################
#Loading data
#######################
d = read.csv('data.csv')
#including Libraries
library(data.table)
library(tidyr)
library(knitr)
library(dplyr)
#Data Pre processing
#Replacing ) with NA
d = d %>%
  mutate(Quantity = replace(Quantity, Quantity<=0, NA),
            UnitPrice = replace(UnitPrice, UnitPrice<=0, NA))
#Dropping NUll values
d = d %>%
  drop_na()
#Converting vales as Factors
d = d  %>% 
  mutate(InvoiceDate=as.Date(InvoiceDate, '%m/%d/%Y %H:%M'), CustomerID=as.factor(CustomerID), 
         Country=as.factor(Country)) 
# Calculating total amount
d = d  %>% 
  mutate(total_dolar = Quantity*UnitPrice)
#dropping the coloumns
d$StockCode=NULL
d$Description=NULL
d$Quantity=NULL
d$Country=NULL
d$UnitPrice=NULL
#RFM analysis 
#install.packages("didrooRFM")
library(didrooRFM)
customerData <- data.frame(d$InvoiceNo,d$CustomerID,d$InvoiceDate,d$total_dolar)
x=findRFM(customerData, recencyWeight = 4, frequencyWeight = 4,
        monetoryWeight = 4)
x$FinalCustomerClass = as.factor(x$FinalCustomerClass)
x$FinalCustomerClass = as.numeric(x$FinalCustomerClass)
#visualization
boxplot(x$FinalCustomerClass)
hist(x$FinalCustomerClass)
summary(x)
table(x$FinalCustomerClass)

