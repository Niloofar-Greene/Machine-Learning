#K means exercise with Iris dataset

#Loading the dataset
library(datasets)
summary(iris)

#**********************************************
set.seed(30)

iris=iris

# Rescaling data
iris_stand=scale(iris[,1:4])

####################################### Non-hierarchic method ################################## 
# Dividing the data into 3 groups
iris_kmeans=kmeans(iris_stand,3,nstart = 1 )
iris_kmeans
# Cluster means shows the means of 3 groups generated
# Clustering vector (iris_kmeans$cluster) shows the group allocation of each of the observation in the dataset
# within cluster sum of squares  (R2) : between_SS / total_SS which is an indication of the efficacy of the model

#**********************************************
#Adding the kmeans clustering results in the original iris table 
iris$Cluster=iris_kmeans$cluster
iris_clus1=iris[iris$Cluster==1,]

# compare the mean of all variables of cluster 1 with entire dataset using summary
summary(iris_clus1)
summary(iris)

# This line shows how successfully we could replicate the species
# It creates a matrix, where column headers are cluster factors, and row headers are species factors
# As we see, the model was pretty good at regrouping all setosa type iris in one cluster, 
# In cluster 1, versicolor and virginica look alike a lot and there is no variable that can distinguish them  
# The majority of cluster 3 are setosa, and there are 4 verginica that look a lot like setosa
table(iris$Species,iris$Cluster)

################################################## Hierarchic method ####################################################### 
# Distance matrix
table_distance=dist(iris_stand, method = "euclidian")
head(table_distance,20)

#hclust : hierarchical clustering
clust_h=hclust(table_distance, method = "ward.D2")
clust_h
plot(clust_h)

# Creating dendrogram with red boxes around the clusters 
rect.hclust(clust_h, k=3, border="red")
dev.off()

#Diviging the data into 3 clusters
groups=cutree(clust_h,3)
plot(clust_h)

# Adding the hierarchical method clusters to the data
iris$Cluster_H=groups

# Checking on how effective are the clusters created in hierarchical method 
# Group 1 clearly distinguish setosa, Group 2 distinguish versicolor 
# Group 3 is a mix of virginica & versicol, so there are some  virginica et versicolor  that look alike alot,
table(iris$Species,iris$Cluster_H)

# comparer les resultsts de Hierachical(H) et Non-Hierarchical(NH) clusters efficacy
# The results show that if I select 3 clusters, the results of H and NH methods will be different
table(iris$Species,iris$Cluster)
table(iris$Species,iris$Cluster_H)

# If I have the time and budget, it is better to do the H method first to decide the number of groups then
# do a Non-hierarchical k-means clustering using the that same number of clusters 

