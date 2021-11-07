library(readr)
library(dplyr)

data <- read_csv("Online_Retail.csv")
data$CustomerID=as.factor(data$CustomerID)

### Nettoyage de donnees 
# 1- Enlever "C" ET "A" au debut de InvoiceNo, car il doit etre un numero
a=grep("^[A-Z]", data$InvoiceNo)
data$tempo=data$InvoiceNo
data$tempo[a]=substr(data$tempo[a], 2, 8)
data$InvoiceNo=data$tempo
data=data[-9]

# 2- enlever les quantites negative
# j'ai remarque que les InvoiceID qui commencetn par C obt les quantite negative et A sont les objet abandonnes.
# donc, les 2 sont a enlever anyways => on enleve les quantites negatives pour nettoyer les donnees
data=subset(data, Quantity>=0)

# 3- to lower case
data$Description=tolower(data$Description)

# On peut garder CustomerID=NA, car on n'apas besoin de CustomerID pour faire notre analyse

# 4 enlever discount
unique(data$StockCode)
data=subset(data, StockCode!="D")

# a) Quel est le produit qui se retrouve le plus au sein des transactions?
B=summarise(group_by(data, StockCode), Count=length(StockCode))
B=B[order(B$Count, decreasing = TRUE),]
C=summarise(group_by(data, StockCode, Description), Count=length(Description))
subset(C, C$StockCode==B$StockCode[1])

# b) Quelle est la provenance engendrant le plus de transactions ? 
B=summarise(group_by(data, Country), Count=length(InvoiceNo))
B=B[order(B$Count, decreasing = TRUE),]
B$Country[1]

# c) Quel est le produit le plus rentable au sein des transactions ?
data$paid=data$Quantity*data$UnitPrice
B=summarise(group_by(data, StockCode),times_perchased=length(StockCode), rentabilite=sum(paid))
B=B[order(B$rentabilite, decreasing = TRUE),]
subset(B, B$StockCode==B$StockCode[c(1,2)])[c(1,2)]

# d) Quel est le client ayant fait le plus de visites ?
B=summarise(group_by(data, CustomerID), Count=length(unique(InvoiceDate)))
B=B[order(B$Count, decreasing = TRUE),]
B$CustomerID[1]

# enregistrer le fichier nettoye
saveRDS(data, file = "clean_data.rds")
