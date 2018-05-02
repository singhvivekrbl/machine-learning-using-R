#loading required libraries
library(lubridate)
     library dplyr,
      librarytidyr,
      libraryBoruta,
      libraryggplot2,
      librarygridExtra,
     library caret,
library (rpart.plot)
library (caTools)
library (doMC)
data <- read.csv("../input/No-show-Issue-Comma-300k.csv", stringsAsFactors = FALSE)
str(data)

data$Gender <- factor(data$Gender, levels = c("M", "F"))
data$AppointmentRegistration <- ymd_hms(data$AppointmentRegistration)
data$ApointmentData <- ymd_hms(data$ApointmentData)
data$DayOfTheWeek <- factor(data$DayOfTheWeek, 
                            levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday" , 
                                     "Saturday", "Sunday"))
# some models don't like levels with character "-", so we apply make.names
data$Status <- factor(make.names(data$Status))
data$Diabetes <- as.logical(data$Diabetes)
data$Alcoolism <- as.logical(data$Alcoolism)
data$HiperTension <- as.logical(data$HiperTension)
data$Handcap <- as.logical(data$Handcap)
data$Smokes <- as.logical(data$Smokes)
data$Scholarship <- as.logical(data$Scholarship)
data$Tuberculosis <- as.logical(data$Tuberculosis)
data$Sms_Reminder <- as.logical(data$Sms_Reminder)

summary(data)


##It looks like there aren't missing values (NA), but some variables have strange
##values which can come from errors (*Age* , *AwaitingTime*) 
## Age
range(data$Age)
sum(data$Age<0)

##We remove the clear Age outlier data:
data <- data[data$Age>0,]

## AwaitingTime,we consider the AwaitingTime as positive values. 

data$AwaitingTime <- abs(data$AwaitingTime)

range(data$AwaitingTime)

str(data)

summary(data)


##Let's check if there are duplicated data

dup_rows <- duplicated(data)
dup_rows_num <- sum(dup_rows)
dup_rows_num

#It looks like there are duplicated rows. Perhaps duplicated are two or more 
#appointment for the same person or perhaps these are errors. As it is a very small
#of rows we will keep them at the moment.
## Status
#Let's see the distribution of "No-Show" and "Shop-up" cases

status_table <- table(data$Status)
status_table
ggplot(data, aes(x=Status, fill=Status)) + geom_bar()

##The porcentage of people who don't show it is very high:

(status_table["No.Show"]/status_table["Show.Up"])*100


##Let's check the other variables:  ApointmentData
##It looks like the proportion of "No-show" each day stays approximately constant through the time:
data %>% group_by(ApointmentData) %>% summarise(total_noshow=sum(Status=="No.Show")/n()) %>% ggplot(aes(x=ApointmentData, y=total_noshow)) + 
geom_point(alpha=0.3) + geom_smooth(method = "lm")
## AppointmentRegistration 
##The dataset variable *AppointmentRegistration* contains the date and the hour of the appointment registration. 


data %>% 
  group_by(RegistrationDate=as.Date(AppointmentRegistration)) %>% 
  summarise(total_noshow=sum(Status=="No.Show")/n()) %>%
  ggplot(aes(x=RegistrationDate, y=total_noshow)) + geom_point(alpha=0.3) + geom_smooth(method = "lm")

#Let's take a look to the hour of the registration appointment:
#It looks like docstors round the hour of the appointment registration, that explains so many picks here:
ggplot(data, aes(x=hour(AppointmentRegistration), fill=Status)) + geom_density() + facet_grid(.~Status)


data %>%
  group_by(RegistrationHour=hour(AppointmentRegistration)) %>%
  summarise(total_noshow=sum(Status=="No.Show")/n()) %>%
  ggplot(aes(x=RegistrationHour, y=total_noshow, fill=as.factor(RegistrationHour))) + geom_bar(stat="identity") + scale_fill_discrete("Registration Hour")

  
people_at5 <- data %>% filter(hour(AppointmentRegistration)>=5 , hour(AppointmentRegistration)<6)

## Age

g_Age_1 <- ggplot(data, aes(x=Age)) + geom_histogram(bins=40)
g_Age_2 <- ggplot(data, aes(x=Status, y=Age, col=Status)) + geom_boxplot()
grid.arrange(g_Age_1, g_Age_2,ncol=2, top='Age distribution, outliers and Status implication')

##It looks like younger people no-show more than older ones.
## Gender
tab_Gender <- table(data$Gender, data$Status)
addmargins(tab_Gender)
prop.table(tab_Gender,2)

g_Gender_1 <- ggplot(data, aes(x=Gender, fill=Gender)) + geom_bar(position="dodge")
g_Gender_2 <- ggplot(data, aes(x=Gender, fill=Status)) + geom_bar(position="fill")
grid.arrange(g_Gender_1, g_Gender_2,ncol=2, top='Gender distribution')

#### DayOfTheWeek
ggplot(data, aes(x=DayOfTheWeek, fill=DayOfTheWeek )) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))



tab_DayOfTheWeek <- table(data$Status, data$DayOfTheWeek)
addmargins(tab_DayOfTheWeek)
prop.table(tab_DayOfTheWeek,2)

ggplot(data, aes(x=DayOfTheWeek, fill=Status )) + geom_bar(position="fill") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

##The day with less "No-show" is Thursday, but it is not representative as it has very few appointments

data %>% group_by(DayOfTheWeek) %>% 
summarise(noshow_prop=sum(Status=="No.Show")/n()) %>% 
ggplot(aes(x=DayOfTheWeek, y=noshow_prop, fill=DayOfTheWeek)) + 
geom_bar(stat="identity") + 
theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Sms_Reminder

tab_Sms <- table(data$Sms_Reminder, data$Status)
addmargins(tab_Sms)
prop.table(tab_Sms,2)

ggplot(data, aes(x=Sms_Reminder, fill=Status)) + geom_bar(position="fill")

## AwaitingTime

##It looks like the last variable (*AwaitingTime*) is the difference between the variables *AppointmentRegistration* and *ApointmentData* 
 
dif_time <- abs(floor(difftime(data$AppointmentRegistration, data$ApointmentData, units = "days")))
sum(dif_time != data$AwaitingTime)

summary(data[data$Status=="No.Show","AwaitingTime"])

summary(data[data$Status=="Show.Up","AwaitingTime"])

g_AwaitingTime_1 <- ggplot(data, aes(x=Status, y=AwaitingTime, col=Status)) + geom_boxplot()
g_AwaitingTime_2 <- ggplot(data, aes(x=AwaitingTime, fill=Status)) + 
  geom_density(alpha=0.30) + 
  coord_cartesian(xlim=c(0, 100))

grid.arrange(g_AwaitingTime_1, g_AwaitingTime_2,ncol=2, top='AwaitingTime distribution')

##Most of the appointments have an awaiting time less than 20. 
agregated_AwaitingTime <- data %>% group_by(AwaitingTime) %>%     
  summarise(No.Show=sum(Status=="No.Show"), 
            Show.Up=sum(Status=="Show.Up"), 
            total=n(), proportion=No.Show/Show.Up )
ggplot(agregated_AwaitingTime, aes(x=AwaitingTime, y=proportion)) + geom_point(alpha=0.4)

#### Diabetes, Alcoolism, HiperTension, Handcap, Smokes, Scholarship, Tuberculosis

g_Diabetes <- ggplot(data, aes(x=Diabetes, fill=Status)) + geom_bar(position="fill")
g_Alcoolism <- ggplot(data, aes(x=Alcoolism, fill=Status)) + geom_bar(position="fill")
g_HiperTension <- ggplot(data, aes(x=HiperTension, fill=Status)) + geom_bar(position="fill")
g_Handcap <- ggplot(data, aes(x=Handcap, fill=Status)) + geom_bar(position="fill")
g_Smokes <- ggplot(data, aes(x=Smokes, fill=Status)) + geom_bar(position="fill")
g_Scholarship <- ggplot(data, aes(x=Scholarship, fill=Status)) + geom_bar(position="fill")
g_Tuberculosis <- ggplot(data, aes(x=Tuberculosis, fill=Status)) + geom_bar(position="fill")

g_binary <- c(g_Diabetes, g_Alcoolism, g_HiperTension, g_Handcap, g_Smokes,
g_Scholarship, g_Tuberculosis)
grid.arrange(g_Diabetes, g_Alcoolism, g_HiperTension, g_Handcap, ncol=2, top='Binary variables effect (1/2)')

grid.arrange(g_Smokes, g_Scholarship, g_Tuberculosis, ncol=2, top='Binary variables effect (2/2)')

### Feature engineering

data$ApointmentMonth <- month(data$ApointmentData,label = TRUE)
ggplot(data, aes(x=ApointmentMonth, fill=Status)) + geom_bar(position="fill")

##It looks like that there are differences among months, but there are not hugh differences
##We will keep the new variable *ApointmentMonth* at the moment because it could be important
# This code takes a lot of time
boruta_results <- Boruta(Status~.-AppointmentRegistration-ApointmentData, data)
boruta_results
plot(boruta_results)



#### Classification tree (rpart)

set.seed(1234)
split_data <- createDataPartition(data$Status, p = 0.7, list = FALSE)
train_data <- data[split_data,]
test_data <- data[-split_data,]

fitControl <- trainControl(method = "cv",
number = 5,
#savePredictions="final",
summaryFunction = twoClassSummary,
classProbs = TRUE
)

train_data <- upSample(train_data[, setdiff(names(data), 'Status')], train_data$Status, yname="Status")
table(train_data$Status)

fit_rpart <- train(Status~.-AppointmentRegistration-ApointmentData-Handcap-Tuberculosis, 
train_data,
method = "rpart",
metric = "ROC",
#preProc = c("center", "scale"),
trControl = fitControl)

pred_rpart <- predict(fit_rpart, test_data)
confusionMatrix(pred_rpart, test_data$Status)

pred_rpart2 <- predict(fit_rpart, test_data, type="prob")
colAUC(pred_rpart2, test_data$Status, plotROC=TRUE)

rpart.plot(fit_rpart$finalModel, type = 2, fallen.leaves = F, cex = 1, extra = 2)

fit_glm <- train(Status~.-AppointmentRegistration-ApointmentData-Handcap-Tuberculosis, 
train_data,
method = "glm",
metric = "ROC",
preProc = c("center", "scale"),
trControl = fitControl)

pred_glm <- predict(fit_glm, test_data)
confusionMatrix(pred_glm, test_data$Status)
summary(fit_glm$finalModel)

pred_glm2 <- predict(fit_glm, test_data, type="prob")
colAUC(pred_glm2, test_data$Status, plotROC=TRUE)

summary(fit_glm)$coef %>% 
as.data.frame() %>% 
cbind(feature=rownames(summary(fit_glm)$coef)) %>% 
filter (.[[4]] <= 0.05) %>% 
arrange(desc(abs(Estimate)), desc(.[[4]]))
