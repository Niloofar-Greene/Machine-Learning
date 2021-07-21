library(readr)
library(arules)
library(tidyr)
library(dplyr)

data <- read_csv("Online_Retail_3.csv")
data$CustomerID=as.factor(data$CustomerID)

### Nettoyage de donnees 
# Enlever "C" au debut de InvoiceNo, car il doit etre un numero
data_1=data
a=grep("^[A-Z]", data_1$InvoiceNo)
data_1$XXX=data_1$InvoiceNo
data_1$XXX[a]=substr(data_1$XXX[a], 2, 8)
data_1$InvoiceNo=data_1$XXX
data_1=data_1[-9]

# 1- to lower case
data_1$Description=tolower(data_1$Description)
# 2- enlever CustomerID=NA
data_1=subset(data_1, !is.na(CustomerID))
# 3- enlever Quantity negative
data_1=subset(data_1, Quantity>=0)
# removing extra columns
data_1=data_1[,-c(3,5:8)]

# what are the number of items available?
length(unique(data_1$StockCode))
# is there one quantity for each item in the list for each invoice number?
B=summarise(group_by(data_1, InvoiceNo), item_count=length(StockCode))

#Adding achat column
data_1$achat=ifelse(data_1$Quantity>0, 1,0)

# changer le format des données en format transactionnel: https://uc-r.github.io/tidyr
df_Trans <-  as.data.frame(gather(data_1, key="mesItem", value="achat", -c("transactionID")))
# les 25 permieres items qui ont la plus grande frequence (support)
itemFrequencyPlot(data_1, topN=25)

