############################
# loading required libraries
############################
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(plotly)
library(knitr)
library(stringr)
library(tm)
library(RWeka)
library(wordcloud)
library(formattable)
#####################################
## reading data set file

isis <- read.csv("../input/tweets.csv")
str(isis)
isis %>% mutate(isRT = grepl("^\\RT\\b", tweets)) -> isis
isis %>% summarize("Tweets" = nrow(subset(isis, isRT == FALSE)), 
                   "Retweets" = nrow(subset(isis, isRT == TRUE))) %>%
  formattable(align = "c") 
isis$time <- strptime(isis$time, format="%m/%d/%Y  %H:%M")

isis <- mutate(isis, days = as.Date(format(time, "%m/%d/%Y"), "%m/%d/%Y"))

isis %>% ggplot(aes(days)) + geom_bar(fill = "lightblue") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Number of tweets published every day") + xlab("") + ylab("") -> g
ggplotly(g)

isis$days %>% as.yearmon %>% table %>% as.data.frame -> monthlytweets

isis$days %>% table %>% sort(decreasing = TRUE) %>% head(10) %>% as.data.frame -> top10days

actual <- subset(isis, isRT == FALSE & days %in% as.Date(rownames(top10days)))
actual$days %>% table %>% as.data.frame -> actual10days
actual10days <- actual10days[match(rownames(top10days), actual10days$.), ]

top10days$Date <- rownames(top10days) 
top10days$Actual_Tweets <- actual10days$Freq
top10days <- rename(top10days, Tweets = .)
top10days <- mutate(top10days, RT = Tweets - Actual_Tweets)
top10days <- top10days[c("Date", "Tweets", "Actual_Tweets", "RT")]
kable(top10days, align = "l")

##arranger
cloud <- subset(isis, isRT == FALSE)

cloud$tweets <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", cloud$tweets) 
cloud$tweets <- gsub("[[:punct:]]", "", cloud$tweets)
cloud$tweets <- gsub("[[:digit:]]", "", cloud$tweets)
cloud$tweets <- gsub("http\\w+", "", cloud$tweets)
cloud$tweets <- gsub("[ \t]{2,}", "", cloud$tweets)
cloud$tweets <- gsub("^\\s+|\\s+$", "", cloud$tweets)
cloud$tweets <- gsub("amp", "", cloud$tweets)
cloud$tweets <- tolower(cloud$tweets)
cloud$tweets <- removeWords(cloud$tweets, stopwords("english"))
cloud$tweets <- gsub("english translation", "", cloud$tweets)

isiscorp <- Corpus(VectorSource(cloud))

freq <- function(x){
  tdm <- TermDocumentMatrix(x)
  ngramFreq <- as.data.frame(rowSums(as.matrix(tdm)))
  ngramFreq <- cbind(rownames(ngramFreq), ngramFreq)
  colnames(ngramFreq) <- c("Words", "Frequency")
  ngramFreq <- arrange(ngramFreq, desc(Frequency))
  ngramFreq
}

tweets <- Corpus(VectorSource(isiscorp[[8]]$content))
tweetsFreq <- freq(tweets)

cloud <- function(x){
  wordcloud(x$Words, x$Frequency, max.words = 100, random.order = FALSE, rot.per = .35, colors = brewer.pal(8, "Dark2"))
}

cloud(tweetsFreq)

isis <- mutate(isis, hash = str_extract_all(tweets, pattern = "#\\S+"))
hashtags <- unlist(isis$hash) %>% table %>% sort(decreasing = TRUE) %>% head(10) %>% as.data.frame
colnames(hashtags) <- "Frequency"
kable(hashtags, align = "c")

tophash <- function(i){
  tophash <- subset(isis, days == top10days$Date[i]) %>% select(hash) %>% unlist %>% table %>% sort(decreasing = TRUE) %>% head(10) %>% as.data.frame 
  kable(tophash, col.names = top10days$Date[i], align = "c")
}
How do the tweets flow evolves over time?What do these users say? Who are the users?
sapply(1:5, tophash)
rename(monthlytweets, Month = ., Tweets = Freq) %>% formattable(list(Tweets = color_bar("lightblue")), align = "l")
