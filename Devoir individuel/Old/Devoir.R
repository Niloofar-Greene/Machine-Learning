data <- read_csv("Online_Retail_3.csv")

### Nettoyage de donnees 
# Enlever "C" au debut de InvoiceNo, car il doit etre un numero
data_1=data
a=grep("^[A-Z]", data_1$InvoiceNo)
data_1$XXX=data_1$InvoiceNo
data_1$XXX[a]=substr(data_1$XXX[a], 2, 8)
data_1$InvoiceNo=data_1$XXX
data_1$XXX=NULL

# 1- to lower case
data_1$Description=tolower(data_1$Description)
# 2- enlever CustomerID=NA
data_1=subset(data_1, !is.na(CustomerID))

length(unique(data_1$StockCode))==length(unique(data_1$Description))
length(unique(data_1$StockCode))
length(unique(data_1$Description))

library(dplyr)
B=summarise(group_by(data_1, StockCode, Description), pourcentage=length(Description))
C=summarise(group_by(B,StockCode), count=n())
C=C[order(C$count, decreasing = TRUE),]
C=subset(C, count >1)

# 2- remove wrongly 
data_2=data_1
grep("*wrongly",data_2$Description)
data_2=data_2[-grep("*wrongly",data_2$Description),]

B=summarise(group_by(data_2, StockCode, Description), pourcentage=length(Description)/length(StockCode))
C=summarise(group_by(B,StockCode), count=n())
C=C[order(C$count, decreasing = TRUE),]
C=subset(C, count >1)
