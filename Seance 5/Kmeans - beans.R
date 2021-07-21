library(readxl)
data <- read_excel("Dry_Bean_Dataset.xlsx")
summary(data)
str(data)

#**********************************************
set.seed(30)

data2=data[,2:17]

data_stand=scale(data2)
####################################### methode non-hierarchique - k-means ################################## 
unique(data$Class) # 7 groups
# 7 groups
data_kmeans=kmeans(data_stand,7,nstart = 1 )
data_kmeans

#**********************************************
#rajouter les resultats de k-means dans la table originale
data$Cluster=data_kmeans$cluster

table(data$Class,data$Cluster)

################################################## methode hierarchique #################################### 
# distance matrix
table_distance=dist(data_stand, method = "euclidian")
head(table_distance,20)

#hclust : hierarchical clustering
clust_h=hclust(table_distance, method = "ward.D2")
clust_h
plot(clust_h)
rect.hclust(clust_h, k=7, border="red") #here I confirm tha tthe choise of 7 groups is good

# cut tree into 7 clucters
groups=cutree(clust_h,7)

data$Cluster_H=groups

##################################### comparing H and NH method results ################################ 
table(data$Class,data$Cluster_H)
table(data$Class,data$Cluster)
