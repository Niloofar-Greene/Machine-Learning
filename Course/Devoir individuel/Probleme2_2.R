library(readr)
library(arules)
library(tidyr)

# Loader le fichier nettoye enregistre 
data=readRDS("clean_data.rds")

# Enlever les columnes qui ne sont pas necessaires pour cette analyse
data=data[,c(1,2)]

# combien d'item existe-il au magasin?
n_item=length(unique(data$StockCode))

# rename coloumn names
names(data)[2]="item"

write.csv(data_1,
          "donneesTransac.csv",
          row.names = F)

trans <- read.transactions(
  file = "donneesTransac.csv",
  format = "single",
  sep = ",", header = T,
  cols=c("InvoiceNo","item"),
  rm.duplicates = T
)

rm(data)

inspect(trans[1:5])

# les 25 permieres items qui ont la plus grande frequence (support)
itemFrequencyPlot(trans, topN=25)
# 2eme solution pour probleme2_1 a)
sort(itemFrequency(trans), decreasing = TRUE)[1]

# Rule d'association
assoc_rules <- apriori(trans, parameter = list(supp=0.024, conf=0.7,maxlen=5))
Apriori_df = inspect(assoc_rules)
inspect(sort(assoc_rules,by="lift"))
summary(assoc_rules)



