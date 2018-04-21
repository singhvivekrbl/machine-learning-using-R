##calling libraies
library('tidyr')
library('ggplot2')
library('gains')
bankcampaigndata <- read.csv('bankdatafile.csv')
mydatac <- bankcampaigndata
print("No of Observation in the File:\n")
nrow(bankcampaigndata)

bankcampaigndata$ACC_OP_DATE <- as.Date(bankcampaigndata$ACC_OP_DATE,"%m/%d/%y")
str(bankcampaigndata)
bankcampaigndata <- within(bankcampaigndata, rm('AGE_BKT'))  ## removing the age bracket column as this seems to redundent here

print("Categorisation of Target Data:\n")

table(bankcampaigndata$TARGET)  ## Data seems to be biassed here
bankcampaigndata_analysis <- bankcampaigndata ## taking data into anothet frame for further manipulation
summary(bankcampaigndata)
###selecting data and running query on data frame

banking_duplicate_Indicator <- sqldf('SELECT count(cust_id),CUST_ID FROM bankcampaigndata GROUP BY CUST_ID ORDER BY CUST_ID')
write.csv(banking_duplicate_Indicator, file = "banking_duplicate_Indicator.csv")

qqmysummary <- sqldf('select AGE_BKT, sum(NO_OF_L_CR_TXNS) "No. of Credit Transactions",sum(NO_OF_L_DR_TXNS) "No. of Debit Transactions", sum(NO_OF_ATM_DR_TXNS) "No. of ATM Debit Transactions",sum(NO_OF_NET_DR_TXNS) "No. of Net Debit Transactions",sum(NO_OF_MOB_DR_TXNS) "No. of Mobile Banking Debit Transactions",sum(NO_OF_CHQ_DR_TXNS) "No. of Cheque Debit Transactions", avg(AVG_AMT_PER_ATM_TXN) "Avg. Amt withdrawn per ATM Transaction", avg(AVG_AMT_PER_CSH_WDL_TXN) "Avg. Amt withdrawn per Cash Withdrawal Transaction",avg(AVG_AMT_PER_CHQ_TXN) "Avg. Amt debited per Cheque Transaction", avg(AVG_AMT_PER_NET_TXN) "Avg. Amt debited per Net Transaction", avg(AVG_AMT_PER_MOB_TXN) "Avg. Amt debited per Mobile Banking Transaction" FROM mydatac group by AGE_BKT ORDER by AGE_BKT')
write.csv(qqmysummary, file = "qqsumamry.csv")



# sqldf('SELECT count(cust_id),CUST_ID FROM bankcampaigndata GROUP BY CUST_ID ORDER BY CUST_ID')
# sqldf('SELECT distinct OCCUPATION FROM bankcampaigndata ')
mysummary <- sqldf('select AGE_BKT, sum(NO_OF_L_CR_TXNS) "No. of Credit Transactions",sum(NO_OF_L_DR_TXNS) "No. of Debit Transactions", sum(NO_OF_ATM_DR_TXNS) "No. of ATM Debit Transactions",sum(NO_OF_NET_DR_TXNS) "No. of Net Debit Transactions",sum(NO_OF_MOB_DR_TXNS) "No. of Mobile Banking Debit Transactions",sum(NO_OF_CHQ_DR_TXNS) "No. of Cheque Debit Transactions", sum(AVG_AMT_PER_ATM_TXN) "Avg. Amt withdrawn per ATM Transaction", sum(AVG_AMT_PER_CSH_WDL_TXN) "Avg. Amt withdrawn per Cash Withdrawal Transaction",sum(AVG_AMT_PER_CHQ_TXN) "Avg. Amt debited per Cheque Transaction", sum(AVG_AMT_PER_NET_TXN) "Avg. Amt debited per Net Transaction", sum(AVG_AMT_PER_MOB_TXN) "Avg. Amt debited per Mobile Banking Transaction" FROM mydatac group by AGE_BKT ORDER by AGE_BKT')
write.csv(mysummary, file = "sumamry.csv")

mynewsummary <- sqldf('select FLG_HAS_OLD_LOAN, sum(NO_OF_L_CR_TXNS) "No. of Credit Transactions",sum(NO_OF_L_DR_TXNS) "No. of Debit Transactions", sum(NO_OF_ATM_DR_TXNS) "No. of ATM Debit Transactions",sum(NO_OF_NET_DR_TXNS) "No. of Net Debit Transactions",sum(NO_OF_MOB_DR_TXNS) "No. of Mobile Banking Debit Transactions",sum(NO_OF_CHQ_DR_TXNS) "No. of Cheque Debit Transactions", sum(AVG_AMT_PER_ATM_TXN) "Avg. Amt withdrawn per ATM Transaction", sum(AVG_AMT_PER_CSH_WDL_TXN) "Avg. Amt withdrawn per Cash Withdrawal Transaction",sum(AVG_AMT_PER_CHQ_TXN) "Avg. Amt debited per Cheque Transaction", sum(AVG_AMT_PER_NET_TXN) "Avg. Amt debited per Net Transaction", sum(AVG_AMT_PER_MOB_TXN) "Avg. Amt debited per Mobile Banking Transaction" FROM mydatac group by FLG_HAS_OLD_LOAN')
write.csv(mynewsummary, file = "newsumamry.csv")


mynewccsummary <- sqldf('select FLG_HAS_CC, sum(NO_OF_L_CR_TXNS) "No. of Credit Transactions",sum(NO_OF_L_DR_TXNS) "No. of Debit Transactions", sum(NO_OF_ATM_DR_TXNS) "No. of ATM Debit Transactions",sum(NO_OF_NET_DR_TXNS) "No. of Net Debit Transactions",sum(NO_OF_MOB_DR_TXNS) "No. of Mobile Banking Debit Transactions",sum(NO_OF_CHQ_DR_TXNS) "No. of Cheque Debit Transactions", sum(AVG_AMT_PER_ATM_TXN) "Avg. Amt withdrawn per ATM Transaction", sum(AVG_AMT_PER_CSH_WDL_TXN) "Avg. Amt withdrawn per Cash Withdrawal Transaction",sum(AVG_AMT_PER_CHQ_TXN) "Avg. Amt debited per Cheque Transaction", sum(AVG_AMT_PER_NET_TXN) "Avg. Amt debited per Net Transaction", sum(AVG_AMT_PER_MOB_TXN) "Avg. Amt debited per Mobile Banking Transaction" FROM mydatac group by FLG_HAS_CC')
write.csv(mynewccsummary, file = "flagccnewsumamry.csv")
bankcampaigndata %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()
## density diagram for all the variables
bankcampaigndata %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_density()                         # as density
## density diagram for all the variables
bankcampaigndata %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_bar() 

## density diagram for all the variables

mycor <- ggcorr(bankcampaigndata)
mycor
bankcampaigndata$BALANCE <- scale(bankcampaigndata$BALANCE)
bankcampaigndata$AMT_ATM_DR <-scale(bankcampaigndata$AMT_ATM_DR)
bankcampaigndata$AVG_AMT_PER_MOB_TXN <-scale(bankcampaigndata$AVG_AMT_PER_MOB_TXN)
bankcampaigndata$AVG_AMT_PER_NET_TXN <-scale(bankcampaigndata$AVG_AMT_PER_NET_TXN)
bankcampaigndata$AVG_AMT_PER_CHQ_TXN <-scale(bankcampaigndata$AVG_AMT_PER_CHQ_TXN)
bankcampaigndata$AVG_AMT_PER_CSH_WDL_TXN <-scale(bankcampaigndata$AVG_AMT_PER_CSH_WDL_TXN)
bankcampaigndata$AVG_AMT_PER_ATM_TXN <-scale(bankcampaigndata$AVG_AMT_PER_ATM_TXN)
bankcampaigndata$AMT_L_DR <-scale(bankcampaigndata$AMT_L_DR)
bankcampaigndata$AMT_MOB_DR <-scale(bankcampaigndata$AMT_MOB_DR)
bankcampaigndata$AMT_NET_DR <-scale(bankcampaigndata$AMT_NET_DR)
bankcampaigndata$AMT_CHQ_DR <-scale(bankcampaigndata$AMT_CHQ_DR)
bankcampaigndata$AMT_BR_CSH_WDL_DR <-scale(bankcampaigndata$AMT_BR_CSH_WDL_DR)
bankcampaigndata$AMT_ATM_DR <-scale(bankcampaigndata$AMT_ATM_DR)
##mydatac$AMT_ATM_DR

bankcampaigndata_analysis <- dummy.data.frame(bankcampaigndata[,-1], sep = ".")
str(bankcampaigndata_analysis)
###changing the column name to meaningfull one
names(bankcampaigndata_analysis)[names(bankcampaigndata_analysis) == "OCCUPATION.PROF"] <- "OCCUPROF"
names(bankcampaigndata_analysis)[names(bankcampaigndata_analysis) == "OCCUPATION.SAL"] <- "OCCUSAL"
names(bankcampaigndata_analysis)[names(bankcampaigndata_analysis) == "OCCUPATION.SELF-EMP"] <- "OCCUSELFEMP"
names(bankcampaigndata_analysis)[names(bankcampaigndata_analysis) == "OCCUPATION.SENP"] <- "OCCUSENP"

### Split the data into test and training dataset.. Here we have assumed ratio as 75% and 25% between training and test dataset
MyIndex  <- sample(1:nrow(bankcampaigndata_analysis), size= 0.25*nrow(bankcampaigndata_analysis))

bankcampaigndata_analysis_test <- bankcampaigndata_analysis[MyIndex,]
nrow(bankcampaigndata_analysis_test)
bankcampaigndata_analysis_training <- bankcampaigndata_analysis[-MyIndex,]
nrow(bankcampaigndata_analysis_training)

str(bankcampaigndata_analysis_training)
bankcampaigndata.aov <- aov(TARGET ~., bankcampaigndata_analysis_training)
summary(bankcampaigndata.aov)
mybankRF <- randomForest(TARGET ~., data= bankcampaigndata_analysis_training, ntree=80, mtry=3, nodesize=7,importance=TRUE)
print(mybankRF)
plot(mybankRF, main="Random Forest Tree Optimal Diagram",col = "dark red")
legend("topright", c("OOB","0","1"), text.col = 1:6,lty = 1:3,col = 1:3)  ## So our optimium no of tree 20
impvar <- round(randomForest::importance(mybankRF),2)
impvar[order(impvar[,2],decreasing = TRUE),]  ### Balance and SCR are most
varImpPlot(mybankRF)  ## PLOTTING THE VI
mybankTuneRF <- tuneRF(x=bankcampaigndata_analysis_training[,-1],  ### so optimised mtry is 5
                       y=bankcampaigndata_analysis_training$TARGET,
                       mtryStart= 3,
                       ntreeTry=20,
                       stepFactor=1.5,
                       improve=0.0001,
                       trace=TRUE,
                       plot = TRUE,
                       doBest = TRUE,
                       nodesize=10,
                       importance=TRUE
)
mybankVIFinalRF <- randomForest(TARGET ~ BALANCE+ SCR+AMT_L_DR+ACC_OP_DATE+HOLDING_PERIOD+
                                  LEN_OF_RLTN_IN_MNTH+TOT_NO_OF_L_TXNS+NO_OF_L_CR_TXNS+AGE+
                                  AMT_BR_CSH_WDL_DR+AVG_AMT_PER_CSH_WDL_TXN+AMT_ATM_DR+
                                  AVG_AMT_PER_ATM_TXN+AVG_AMT_PER_CHQ_TXN+AMT_CHQ_DR+AMT_NET_DR+
                                  AVG_AMT_PER_NET_TXN+NO_OF_L_DR_TXNS+NO_OF_BR_CSH_WDL_DR_TXNS+
                                  NO_OF_CHQ_DR_TXNS+AMT_MOB_DR+AVG_AMT_PER_MOB_TXN+OCCUSELFEMP 
                                , data= bankcampaigndata_analysis_training, ntree=40, mtry=13, 
                                nodesize=7,importance=TRUE)

print(mybankVIFinalRF)
plot(mybankVIFinalRF, main="Vairable of Importance Random Forest Tree Final Optimal Diagram")
print("Tree Sizes are")
treesize(mybankVIFinalRF)
bankcampaigndata_analysis_training$predictVITargetProb <- predict(mybankVIFinalRF,bankcampaigndata_analysis_training, type='response')
bankcampaigndata_analysis_training$predictVITarget <- ifelse(bankcampaigndata_analysis_training$predictVITargetProb <0.5,0,1)

mycamaigntVIable <- table(actualclass=bankcampaigndata_analysis_training$TARGET, predictedclass=bankcampaigndata_analysis_training$predictVITarget)
mycampaignconfusionVImatrix <- confusionMatrix(mycamaigntVIable)
print(mycampaignconfusionVImatrix)
bankcampaigndata_analysis_test$predictVITargetProb <- predict(mybankVIFinalRF,bankcampaigndata_analysis_test, type='response')
bankcampaigndata_analysis_test$predictVITarget <- ifelse(bankcampaigndata_analysis_test$predictVITargetProb <0.5,0,1)

mycamaigntableVITest <- table(actualclass=bankcampaigndata_analysis_test$TARGET, predictedclass=bankcampaigndata_analysis_test$predictVITarget)
mycampaignconfusionmatrixVITest <- confusionMatrix(mycamaigntableVITest)   ### Neg Pred Value is quite less here
print(mycampaignconfusionmatrixVITest)

bankcampaigndata_analysis_test$kpi <- predict(mybankVIFinalRF,bankcampaigndata_analysis_test, type='response')

newdata<- bankcampaigndata_analysis_test[
  with(bankcampaigndata_analysis_test, order(-kpi)),
  ]

mydecile <- decile(newdata$kpi)
mygain <- gains(bankcampaigndata_analysis_test$TARGET, bankcampaigndata_analysis_test$predictVITarget,groups=10)

print(mygain)
plot(mygain)
bankcampaigndata_analysis_test$deciles <- decile(bankcampaigndata_analysis_test$predictVITargetProb)

library(data.table)
tmp_DT = data.table(bankcampaigndata_analysis_test)
h_rank <- tmp_DT[, list(
  cnt = length(TARGET), 
  cnt_resp = sum(TARGET), 
  cnt_non_resp = sum(TARGET == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round (h_rank$cnt_resp / h_rank$cnt,2);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_rel_resp <- round(h_rank$cum_resp / sum(h_rank$cnt_resp),2);
h_rank$cum_rel_non_resp <- round(h_rank$cum_non_resp / sum(h_rank$cnt_non_resp),2);
h_rank$ks <- abs(h_rank$cum_rel_resp - h_rank$cum_rel_non_resp);

View(h_rank)
