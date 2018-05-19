## calling libraries##

library(data.table)
library(dplyr)
library(ggplot2)
#library(stringr)
#library(DT)
library(tidyr)
library(knitr)
library(rmarkdown)
##Load Dataset
df_data <- fread('data.csv')
glimpse(df_data)
##Data Cleaning
df_data <- df_data %>% 
  mutate(Quantity = replace(Quantity, Quantity<=0, NA),
         UnitPrice = replace(UnitPrice, UnitPrice<=0, NA))

df_data <- df_data %>%
  drop_na()
##Recode variables
df_data <- df_data %>% 
  mutate(InvoiceNo=as.factor(InvoiceNo), StockCode=as.factor(StockCode), 
         InvoiceDate=as.Date(InvoiceDate, '%m/%d/%Y %H:%M'), CustomerID=as.factor(CustomerID), 
         Country=as.factor(Country))

df_data <- df_data %>% 
  mutate(total_dolar = Quantity*UnitPrice)

glimpse(df_data)

##Calculate RFM
df_RFM <- df_data %>% 
  group_by(CustomerID) %>% 
  summarise(recency=as.numeric(as.Date("2012-01-01")-max(InvoiceDate)),
            frequenci=n_distinct(InvoiceNo), monitery= sum(total_dolar)/n_distinct(InvoiceNo)) 

summary(df_RFM)

kable(head(df_RFM))
#---------------------------------------------------------------

table(df_data $Country)
countries <- as.data.frame(table(df_data$Country))
names(countries)[names(countries) == 'Var1'] <- 'country'

df_data <- subset(df_data, Country == "United Kingdom")

length(unique(df_data$InvoiceNo))
length(unique(df_data$CustomerID))

# Identify returns
df_data$item.return <- grepl("C", df_data$InvoiceNo, fixed=TRUE)
df_data$purchase.invoice <- ifelse(df_data$item.return=="TRUE", 0, 1)
table(df_data$item.return)
table(df_data$purchase.invoice)
prop.table(table(df_data$item.return))



customers <- as.data.frame(unique(df_data$CustomerID))
names(customers) <- "CustomerID"

df_data$recency <- as.Date("2011-12-10") - as.Date(df_data$InvoiceDate)

# remove returns so only consider the data of most recent *purchase*
temp <- subset(df_data, purchase.invoice == 1)

# Obtain # of days since most recent purchase
recency <- aggregate(recency ~ CustomerID, data=temp, FUN=min, na.rm=TRUE)

# Add recency to customer data
customers <- merge(customers, recency, by="CustomerID", all=TRUE, sort=TRUE)
customers$recency <- as.numeric(customers$recency)



#############
# Frequency #
#############

customer.invoices <- subset(df_data, select = c("CustomerID","InvoiceNo", "purchase.invoice"))
customer.invoices <- customer.invoices[!duplicated(customer.invoices), ]
customer.invoices <- customer.invoices[order(customer.invoices$CustomerID),]
row.names(customer.invoices) <- NULL

# Number of invoices/year (purchases only)
annual.invoices <- aggregate(purchase.invoice ~ CustomerID, data=customer.invoices, FUN=sum, na.rm=TRUE)
names(annual.invoices)[names(annual.invoices)=="purchase.invoice"] <- "frequency"

# Add # of invoices to customers data
customers <- merge(customers, annual.invoices, by="CustomerID", all=TRUE, sort=TRUE)

range(customers$frequency)
table(customers$frequency)

# Remove customers who have not made any purchases in the past year
customers <- subset(customers, frequency > 0)

df_data$Amount <- df_data$Quantity * df_data$UnitPrice

# Aggregated total sales to customer
annual.sales <- aggregate(Amount ~ CustomerID, data=df_data, FUN =sum, na.rm=TRUE)
names(annual.sales)[names(annual.sales)=="Amount"] <- "monetary"

# Add monetary value to customers dataset
customers <- merge(customers, annual.sales, by="CustomerID", all.x=TRUE, sort=TRUE)

# Identify customers with negative monetary value numbers, as they were presumably returning purchases from the preceding year
hist(customers$monetary, main = NULL, labels = TRUE, las = 1, cex.lab = 1.1, cex.axis = 1.1, col = "blue")
customers$monetary <- ifelse(customers$monetary < 0, 0, customers$monetary) # reset negative numbers to zero
hist(customers$monetary, labels = TRUE, type = "count", breaks = 10, las = 1, col = "blue")

####################
# 80% and 20% rule #
####################

customers <- customers[order(-customers$monetary),]

# Apply Pareto Principle (80/20 Rule)
pareto.cutoff <- 0.8 * sum(customers$monetary)
# customers <- customers[,!customers$cumsum]
customers$pareto <- ifelse(cumsum(customers$monetary) <= pareto.cutoff, "Top 20%", "Bottom 80%")
customers$pareto <- factor(customers$pareto, levels=c("Top 20%", "Bottom 80%"), ordered=TRUE)
levels(customers$pareto)
round(prop.table(table(customers$pareto)), 2)

customers <- customers[order(customers$CustomerID),]

###################
# preprocess data #
###################

# Log-transform positively-skewed variables
customers$recency.log <- log(customers$recency)
customers$frequency.log <- log(customers$frequency)
customers$monetary.log <- customers$monetary + 0.1 # can't take log(0), so add a small value to remove zeros
customers$monetary.log <- log(customers$monetary.log)
customers <- customers[complete.cases(customers), ]

# Z-scores
customers$recency.z <- scale(customers$recency.log, center=TRUE, scale=TRUE)
customers$frequency.z <- scale(customers$frequency.log, center=TRUE, scale=TRUE)
customers$monetary.z <- scale(customers$monetary.log, center=TRUE, scale=TRUE)

##################
# visualize data #
##################

library(ggplot2)
library(scales)

# Original scale
scatter.1 <- ggplot(customers, aes(x = frequency, y = monetary))
scatter.1 <- scatter.1 + geom_point(aes(colour = recency, shape = pareto))
scatter.1 <- scatter.1 + scale_shape_manual(name = "80/20 Designation", values=c(17, 16))
scatter.1 <- scatter.1 + scale_colour_gradient(name="Recency\n(Days since Last Purchase))")
scatter.1 <- scatter.1 + scale_y_continuous(label=dollar)
scatter.1 <- scatter.1 + xlab("Frequency (Number of Purchases)")
scatter.1 <- scatter.1 + ylab("Monetary Value of Customer (Annual Sales)")
scatter.1

# Log-transformed
scatter.2 <- ggplot(customers, aes(x = frequency.log, y = monetary.log))
scatter.2 <- scatter.2 + geom_point(aes(colour = recency.log, shape = pareto))
scatter.2 <- scatter.2 + scale_shape_manual(name = "80/20 Designation", values=c(17, 16))
scatter.2 <- scatter.2 + scale_colour_gradient(name="Log-transformed Recency")
scatter.2 <- scatter.2 + xlab("Log-transformed Frequency")
scatter.2 <- scatter.2 + ylab("Log-transformed Monetary Value of Customer")
scatter.2

#####################
# Handling Outliers #
#####################

# How many customers are represented by the two data points in the lower left-hand corner of the plot? 18
delete <- subset(customers, monetary.log < 0)
no.value.custs <- unique(delete$CustomerID)
delete2 <- subset(df_data, CustomerID %in% no.value.custs)
delete2 <- delete2[order(delete2$CustomerID, delete2$InvoiceDate),]
customers$recency.z <- as.numeric(customers$recency.z)
customers$frequency.z <- as.numeric(customers$frequency.z)
customers$monetary.z <- as.numeric(customers$monetary.z)
# Scaled variables
scatter.3 <- ggplot(customers, aes(x = customers$frequency.z, y = customers$monetary.z))
scatter.3 <- scatter.3 + geom_point(aes(colour = recency.z, shape = pareto))
scatter.3 <- scatter.3 + scale_shape_manual(name = "80/20 Designation", values=c(17, 16))
scatter.3 <- scatter.3 + scale_colour_gradient(name="Z-scored Recency")
scatter.3 <- scatter.3 + xlab("Z-scored Frequency")
scatter.3 <- scatter.3 + ylab("Z-scored Monetary Value of Customer")
scatter.3

###########################################
# Determine number of cluster/run k-means #
###########################################

# subset only rfm features
preprocessed <- customers[,9:11]

j <- 10 # specify the maximum number of clusters you want to try out

models <- data.frame(k=integer(),
                     tot.withinss=numeric(),
                     betweenss=numeric(),
                     totss=numeric(),
                     rsquared=numeric())

for (k in 1:j ) {
  
  print(k)
  
  # Run kmeans
  # nstart = number of initial configurations; the best one is used
  # $iter will return the iteration used for the final model
  output <- kmeans(preprocessed, centers = k, nstart = 20)
  
  # Add cluster membership to customers dataset
  var.name <- paste("cluster", k, sep="_")
  customers[,(var.name)] <- output$cluster
  customers[,(var.name)] <- factor(customers[,(var.name)], levels = c(1:k))
  
  # Graph clusters
  cluster_graph <- ggplot(customers, aes(x = frequency.z, y = monetary.z))
  cluster_graph <- cluster_graph + geom_point(aes(colour = customers[,(var.name)]))
  colors <- c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1','tan3','black')
  cluster_graph <- cluster_graph + scale_colour_manual(name = "Cluster Group", values=colors)
  cluster_graph <- cluster_graph + xlab("Z-transformed Frequency")
  cluster_graph <- cluster_graph + ylab("Z-transformed Monetary Value of Customer")
  title <- paste("k-means Cluster dengan", k, sep=" ")
  title <- paste(title, "Clusters", sep=" ")
  cluster_graph <- cluster_graph + ggtitle(title)
  print(cluster_graph)
  
  # Cluster centers in original metrics
  library(plyr)
  print(title)
  cluster_centers <- ddply(customers, .(customers[,(var.name)]), summarize,
                           monetary=round(mean(monetary),2),# use median b/c this is the raw, heavily-skewed data
                           frequency=round(mean(frequency),1),# but this is k_means, so we use the mean
                           recency=round(mean(recency), 0))
  names(cluster_centers)[names(cluster_centers)=="customers[, (var.name)]"] <- "Cluster"
  print(cluster_centers)
  cat("\n")
  cat("\n")
  
  # Collect model information
  models[k,("k")] <- k
  models[k,("tot.withinss")] <- output$tot.withinss # the sum of all within sum of squares
  models[k,("betweenss")] <- output$betweenss
  models[k,("totss")] <- output$totss # betweenss + tot.withinss
  models[k,("rsquared")] <- round(output$betweenss/output$totss, 3) # percentage of variance explained by cluster membership
  assign("models", models, envir = .GlobalEnv)
  
  # remove(output, var.name, cluster_graph, cluster_centers, title, colors)
  
}
output1 <- kmeans(preprocessed, centers = 1)
output2 <- kmeans(preprocessed, centers = 2)
output3 <- kmeans(preprocessed, centers = 3)
output4 <- kmeans(preprocessed, centers = 4)
output5 <- kmeans(preprocessed, centers = 5)
output6 <- kmeans(preprocessed, centers = 6)
output7 <- kmeans(preprocessed, centers = 7)
output8 <- kmeans(preprocessed, centers = 8)
output9 <- kmeans(preprocessed, centers = 9)
output10 <- kmeans(preprocessed, centers = 10)

output1$size
output1$centers

output2$size
output2$centers

output3$size
output3$centers

output4$size
output4$centers

output5$size
output5$centers

output6$size
output6$centers

output7$size
output7$centers

output8$size
output8$centers

output9$size
output9$centers

output10$size
output10$centers

fviz_cluster(output2, data = preprocessed, stand = FALSE,
             main = paste("k-means Solution with k = 2"),
             choose.vars = c("frequency.z", "monetary.z"),
             star.plot = FALSE, 
             show.clust.cent = TRUE, 
             geom = "point",palette = "jco", ggtheme = theme_minimal())

# with factoextra
fviz_cluster(output5, data = preprocessed, stand = FALSE,
             main = paste("k-means Solution with k = 5"),
             choose.vars = c("frequency.z", "monetary.z"),
             star.plot = FALSE, 
             show.clust.cent = TRUE, 
             geom = "point",palette = "jco", ggtheme = theme_minimal())


r2_graph <- ggplot(models, aes(x = k, y = rsquared))
r2_graph <- r2_graph + geom_point() + geom_line() + geom_text(aes(label = models$rsquared),hjust=1.15, vjust="outward")
r2_graph <- r2_graph + scale_y_continuous(labels = scales::percent)
 r2_graph <- r2_graph + scale_x_continuous(breaks = 1:j)
r2_graph <- r2_graph + xlab("k (Number of Clusters)")
r2_graph <- r2_graph + ylab("Variance Explained")
r2_graph

# with factoextra
fviz_nbclust(preprocessed, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)

# Graph within sums of squares by number of clusters
# Look for a "bend" in the graph, as with a scree plot
ss_graph <- ggplot(models, aes(x = k, y = tot.withinss))
ss_graph <- ss_graph + geom_point() + geom_line() + geom_text(aes(label = round(models$tot.withinss, 2)),hjust=0, vjust="inward")
ss_graph <- ss_graph + scale_x_continuous(breaks = 1:j)
ss_graph <- ss_graph + scale_y_continuous(labels = scales::comma)
ss_graph <- ss_graph + xlab("k (Number of Clusters)")
ss_graph <- ss_graph + ylab("Total Within SS")
ss_graph
