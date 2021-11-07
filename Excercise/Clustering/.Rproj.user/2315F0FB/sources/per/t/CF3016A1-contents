library(readr)
library(Hmisc)
library(ggplot2)
library(dplyr)

# Load
data <- read_delim("marketing_campaign.csv", "\t", escape_double = FALSE)

# Cleaning
data <- na.omit(data)
data$ID=as.character(data$ID)
data$Dt_Customer=as.POSIXct(data$Dt_Customer, format="%d-%m-%Y")
data <- data[data$Income<600000,]

##Numerical columns
data$age = 2021-data$Year_Birth
min(data$age)
max(data$age)

#creating class out of age
data$age_class=NA

for (i in 1:nrow(data)) {
  if (19<data$age[i] & data$age[i]<30 ) {data$age_class[i]="18-29"
  } else if (29<data$age[i] & data$age[i]<40 ) {
    data$age_class[i]="30-39"
  } else if (39<data$age[i] & data$age[i]<50 ) {
    data$age_class[i]="40-49"
  } else if (49<data$age[i] & data$age[i]<60 ) {
    data$age_class[i]="50-59"
  } else if (59<data$age[i] & data$age[i]<70 ) {
    data$age_class[i]="60-69"
  } else {data$age_class[i]="70+"}
}

##checking if there is a relation between  'Age_class','NumWebPurchases','NumCatalogPurchases','NumStorePurchases','NumDealsPurchase'
summ <- summarize(group_by(data, age_class)
                  , sum_WebPurchases       = round(sum(NumWebPurchases),1)
                  , sum_NumCatalogPurchases= round(sum(NumCatalogPurchases),1)
                  , sum_StorePurchases     = round(sum(NumStorePurchases),1)
                  , sum_DealPurchases      = round(sum(NumDealsPurchases),1)
)

#Loyalty period
data$loyalty_period <- difftime(Sys.Date(),data$Dt_Customer, units="days")
data$loyalty_period <- as.numeric(data$loyalty_period)
data$loyalty_period <- data$loyalty_period/30

#nbr_children
data$nbr_children = data$Kidhome+data$Teenhome

#spending
data$Spending=data$MntWines+data$MntFruits+data$MntMeatProducts+data$MntFishProducts+data$MntSweetProducts+data$MntGoldProds

#Nominal columns
#Education
lab=paste0(round(table(data$Education)/sum(table(data$Education)),2) * 100,"%")
ggplot(data, aes(data$Education)) + 
  geom_bar()
dev.off()

data$Education =recode(data$Education,
                    "Basic"      = "0",
                    "2n Cycle"   = "1",
                    "Graduation" = "2",
                    "Master"     = "3",
                    "PhD"        = "4")
data$Education = as.numeric(data$Education)

#Marital_Status
lab=paste0(round(table(data$Marital_Status)/sum(table(data$Marital_Status)),2) * 100,"%")

data$Marital_Status[data$Marital_Status %in% c("Single","Divorced","Widow","Alone", "Absurd","YOLO")] <-  "Signle"
data$Marital_Status[data$Marital_Status %in% c("Together","Married")] <-  "Couple"

ggplot(data, aes(data$Marital_Status)) + 
  geom_bar()


#finding people with highest spending
ggplot(data, aes(data$Spending)) +
  geom_boxplot()
#The threshold is $1048 

##correlation
data_numeric <- data %>% dplyr::select(where(is.numeric))
res <- round(cor(data_numeric[-grep("Spending", names(data_numeric))],data_numeric$Spending),2)
colnames(res)="Spending"

#remove the unnecessary fields
data <- data[c("age","Education","Income","Marital_Status","nbr_children","loyalty_period","Spending")]

#The following 5 features are the most important when trying to cluster the customer's characteristics
data <- data[c("age","Education","Income","loyalty_period","Spending")]

#Histogram
#age
ggplot(data, aes(data$age)) +   geom_histogram(bins = 100)
#Education
ggplot(data, aes(data$Education)) +   geom_histogram(bins = 100)
#Income
ggplot(data, aes(data$Income)) + geom_histogram(bins = 100)
dev.off()
#loyalty
ggplot(data, aes(data$loyalty_period)) +   geom_histogram(bins = 100)
#Spending
ggplot(data, aes(data$Spending)) +   geom_histogram(bins = 100)

#Scaling
data_stand=scale(data)

####################################### Non-hierarchic (partitioning) Method - k-means ################################## 
# 3 groups
data_kmeans=kmeans(data_stand,3,nstart = 1 )
data_kmeans

#**********************************************
#rajouter les resultats de k-means dans la table originale
data$Cluster_NH=data_kmeans$cluster
data_clus1=data[data$Cluster_NH==1,]

# compare the mean of all variables of cluster1 with entire dataset using summary
summary(data_clus1)
summary(data)

H_Education <- table(data$Education,data$Cluster_NH)

################################################## Hierarchic Method ####################################################### 
# distance matrix
table_distance_euclidian=dist(data_stand, method = "euclidian")
head(table_distance_euclidian,20)
table_distance_manhattan=dist(data_stand, method = "manhattan") #rare to use
head(table_distance_manhattan,20)

#hclust : hierarchical clustering
clust_h=hclust(table_distance, method = "ward.D2")

# Creates dendogram with red boxes around the clusters do the folloing
plot(clust_h)
rect.hclust(clust_h, k=3, border="red")
clust_h

dev.off()

# cut tree into 3 clusters
groups=cutree(clust_h,3)

# H for hierarchic
data$Cluster_H=groups

table(data$Education,data$Cluster_H)
table(data$Education,data$Cluster_NH)

#cONVERT TO FACTOR
data$Cluster_H <- as.factor(data$Cluster_H)
data$Cluster_NH <- as.factor(data$Cluster_NH)

#Hierarchical plotting
p2 <- ggplot(data, aes(x=Education, y=Income, color=Cluster_H) )+
  geom_point()

p2 <- ggplot(data, aes(x=age, y=Income, color=Cluster_H) )+
  geom_point(alpha = 0.9)+ xlim(25, 70)

p2 <- ggplot(data, aes(x=loyalty_period, y=Income, color=Cluster_H) )+
  geom_point()

p2 <- ggplot(data, aes(x=Spending, y=Income, color=Cluster_H) )+
  geom_point()

p2+scale_color_manual(breaks = c("1", "2", "3"),
                      values=c("red", "blue", "green"))

#ggplot- facet - comparing H and NH clustering
require(gridExtra)
#Education-Income
plot1 <- ggplot(data, aes(x=Education, y=Income, color=Cluster_H) )+
  geom_point()+scale_color_manual(breaks = c("1", "2", "3"),
                                  values=c("red", "blue", "green"))
plot2 <- ggplot(data, aes(x=Education, y=Income, color=Cluster_NH) )+
  geom_point()+scale_color_manual(breaks = c("1", "2", "3"),
                                  values=c("red", "blue", "green"))
grid.arrange(plot1, plot2, ncol=2)

#age-Income
plot1 <- ggplot(data, aes(x=age, y=Income, color=Cluster_H) )+
  geom_point(alpha = 0.9)+ xlim(25, 70)+scale_color_manual(breaks = c("1", "2", "3"),
                                                           values=c("red", "blue", "green"))
plot2 <- ggplot(data, aes(x=age, y=Income, color=Cluster_NH) )+
  geom_point(alpha = 0.9)+ xlim(25, 70)+scale_color_manual(breaks = c("1", "2", "3"),
                                                           values=c("red", "blue", "green"))
grid.arrange(plot1, plot2, ncol=2)

#Income-Loyaulty period
plot1 <- ggplot(data, aes(x=loyalty_period, y=Income, color=Cluster_H) )+
  geom_point()+scale_color_manual(breaks = c("1", "2", "3"),
                                  values=c("red", "blue", "green"))
plot2 <- ggplot(data, aes(x=loyalty_period, y=Income, color=Cluster_NH) )+
  geom_point()+scale_color_manual(breaks = c("1", "2", "3"),
                                  values=c("red", "blue", "green"))
grid.arrange(plot1, plot2, ncol=2)



