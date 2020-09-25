
EngData <-read.csv(file.choose())
str(EngData)
EngData$Teaching <-as.integer(EngData$Teaching)
EngData$Fees <-as.integer(EngData$Fees)
EngData$Placements <-as.integer(EngData$Placements)
EngData$Internship <-as.integer(EngData$Internship)
EngData$Infrastructure <-as.integer(EngData$Infrastructure)
str(EngData)

Scale_Engdata = scale(EngData[3:7],scale = TRUE) 
View(Scale_Engdata)
d.euc <- dist(x=Scale_Engdata, method = "euclidean")

d.euc
clust1 <- hclust(d.euc, method = "average")
plot(clust1, labels = as.character(EngData[,2]))

View(EngData)
## profiling the clusters
EngData$Clusters <- cutree(clust1, k=2)
agg = aggregate(EngData[,-c(1,2, 8)],list(EngData$Clusters),mean)
clust.profile <- data.frame( Cluster=agg[,1],
                            Freq=as.vector(table(EngData$Clusters)),
                            agg[,-1])
View(clust.profile)
View(EngData)
##----------------Part 2-----------
data_kmeans <- read.csv('Engg_College_Data.csv', header=TRUE)
View(data_kmeans)
## scale function standardizes the values
scaled_kmeans <- scale(data_kmeans[,3:7])

View(scaled_kmeans)

## Identifying the optimal number of clusters form WSS

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(scaled_kmeans, nc=5)

## Identifying the optimal number of clusters
##install.packages("NbClust")

library(NbClust)
?NbClust

set.seed(1234)
nc <- NbClust(data_kmeans[,c(-1,-2)], min.nc=2, max.nc=5, method="kmeans")
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")


?kmeans
kmeans.clus = kmeans(x=scaled_kmeans, centers = 2)
kmeans.clus



## profiling the clusters
data_kmeans$Clusters <- kmeans.clus$cluster
View(data_kmeans)
aggr = aggregate(data_kmeans[,-c(1,2, 8)],list(data_kmeans$Clusters),mean)
clus.profile <- data.frame( Cluster=aggr[,1],
                            Freq=as.vector(table(data_kmeans$Clusters)),
                            aggr[,-1])

View(clus.profile)


## plotting the clusters
##install.packages("fpc")
library(fpc)
plotcluster(scaled_kmeans, kmeans.clus$cluster)

# More complex
library(cluster)
?clusplot
clusplot(scaled_kmeans, kmeans.clus$cluster, 
         color=TRUE, shade=TRUE, labels=2, lines=1)


##############################################################
##     Part 3                                               ##
##############################################################

## method = ward.D
clus2 <- hclust(d.euc, method = "ward.D")
plot(clus2, labels = as.character(EngData[,2]))
rect.hclust(clus2, k=2, border="red")
clus2$height


EngData$Clusters <- cutree(clus2, k=2)
View(EngData)

## profiling the clusters
aggr = aggregate(EngData[,-c(1,2, 8)],list(EngData$Clusters),mean)
clus.profile <- data.frame( Cluster=aggr[,1],
                            Freq=as.vector(table(EngData$Clusters)),
                            aggr[,-1])

View(clus.profile)

## Method = ward.D2
clus2 <- hclust(d.euc, method = "ward.D2")
plot(clus2, labels = as.character(my_data[,2]))
rect.hclust(clus2, k=2, border="red")
clus2$height


my_data$Clusters <- cutree(clus2, k=2)
View(my_data)

## profiling the clusters
aggr = aggregate(my_data[,-c(1,2, 8)],list(my_data$Clusters),mean)
clus.profile <- data.frame( Cluster=aggr[,1],
                            Freq=as.vector(table(my_data$Clusters)),
                            aggr[,-1])

View(clus.profile)

##Method = complete
clus2 <- hclust(d.euc, method = "complete")
plot(clus2, labels = as.character(EngData[,2]))
rect.hclust(clus2, k=2, border="red")
clus2$height


EngData$Clusters <- cutree(clus2, k=2)
View(EngData)

## profiling the clusters
aggr = aggregate(EngData[,-c(1,2, 8)],list(EngData$Clusters),mean)
clus.profile <- data.frame( Cluster=aggr[,1],
                            Freq=as.vector(table(EngData$Clusters)),
                            aggr[,-1])

View(clus.profile)


## method = sinlge
clus2 <- hclust(d.euc, method = "single")
plot(clus2, labels = as.character(EngData[,2]))
rect.hclust(clus2, k=2, border="red")
clus2$height


EngData$Clusters <- cutree(clus2, k=2)
View(EngData)

## profiling the clusters
aggr = aggregate(EngData[,-c(1,2, 8)],list(EngData$Clusters),mean)
clus.profile <- data.frame( Cluster=aggr[,1],
                            Freq=as.vector(table(EngData$Clusters)),
                            aggr[,-1])

View(clus.profile)

## method = mcquitty
clus2 <- hclust(d.euc, method = "mcquitty")
plot(clus2, labels = as.character(my_data[,2]))
rect.hclust(clus2, k=2, border="red")
clus2$height


my_data$Clusters <- cutree(clus2, k=2)
View(my_data)

## profiling the clusters
aggr = aggregate(my_data[,-c(1,2, 8)],list(my_data$Clusters),mean)
clus.profile <- data.frame( Cluster=aggr[,1],
                            Freq=as.vector(table(my_data$Clusters)),
                            aggr[,-1])

View(clus.profile)

#Method = median
clus2 <- hclust(d.euc, method = "median")
plot(clus2, labels = as.character(EngData[,2]))
rect.hclust(clus2, k=2, border="red")
clus2$height
EngData$Clusters <- cutree(clus2, k=2)
View(EngData)
## profiling the clusters
aggr = aggregate(EngData[,-c(1,2, 8)],list(EngData$Clusters),mean)
clus.profile <- data.frame( Cluster=aggr[,1],
                            Freq=as.vector(table(EngData$Clusters)),
                            aggr[,-1])

View(clus.profile)

## Method = centroid
clus2 <- hclust(d.euc, method = "centroid")
plot(clus2, labels = as.character(my_data[,2]))
rect.hclust(clus2, k=2, border="red")
clus2$height


my_data$Clusters <- cutree(clus2, k=2)
View(my_data)

## profiling the clusters
aggr = aggregate(my_data[,-c(1,2, 8)],list(my_data$Clusters),mean)
clus.profile <- data.frame( Cluster=aggr[,1],
                            Freq=as.vector(table(my_data$Clusters)),
                            aggr[,-1])

View(clus.profile)








##-------------------------------------
setwd ("D:/K2Analytics/datafile")
getwd()

RCDF <- read.csv("Cust_Spend_Data.csv", header=TRUE)
View(RCDF)

?dist  ## to get help on distance function
d.euc <- dist(x=RCDF[,3:7], method = "euclidean") 
d.euc

## we will use the hclust function to build the cluster
?hclust  ## to get help on hclust function

clus1 <- hclust(d.euc, method = "average")
plot(clus1, labels = as.character(RCDF[,2]))

## scale function standardizes the values
scaled.RCDF <- scale(RCDF[,3:7])
head(scaled.RCDF, 10)
d.euc
d.euc <- dist(x=scaled.RCDF, method = "euclidean") 
clus2 <- hclust(d.euc, method = "average")
plot(clus2, labels = as.character(RCDF[,2]))
rect.hclust(clus2, k=4, border="red")
clus2$height

View(RCDF)
## profiling the clusters
RCDF$Clusters <- cutree(clus2, k=3)
aggr = aggregate(RCDF[,-c(1,2, 8)],list(RCDF$Clusters),mean)
clus.profile <- data.frame( Cluster=aggr[,1],
                            Freq=as.vector(table(RCDF$Clusters)),
                            aggr[,-1])

View(clus.profile)



## K Means Clustering


KRCDF <- read.csv("Cust_Spend_Data.csv", header=TRUE)
## scale function standardizes the values
scaled.RCDF <- scale(KRCDF[,3:7])

##KRCDF <- read.csv("datafiles/KBD.csv", header=TRUE)
##scaled.RCDF <- scale(KRCDF[,2:3])
View(scaled.RCDF)
class(scaled.RCDF)
## code taken from the R-statistics blog
## http://www.r-statistics.com/2013/08/k-means-clustering-from-r-in-action/

## Identifying the optimal number of clusters form WSS

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(scaled.RCDF, nc=5)

## Identifying the optimal number of clusters
##install.packages("NbClust")

library(NbClust)
?NbClust

set.seed(1234)
nc <- NbClust(KRCDF[,c(-1,-2)], min.nc=2, max.nc=4, method="kmeans")
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
          xlab="Numer of Clusters", ylab="Number of Criteria",
          main="Number of Clusters Chosen by 26 Criteria")


?kmeans
kmeans.clus = kmeans(x=scaled.RCDF, centers = 3, nstart = 25)
kmeans.clus

tmp <- as.data.frame(scaled.RCDF)
tmp$Clusters <- kmeans.clus$cluster
View(tmp)
## plotting the clusters
##install.packages("fpc")
library(fpc)
plotcluster(scaled.RCDF, kmeans.clus$cluster)

# More complex
library(cluster)
?clusplot
clusplot(scaled.RCDF, kmeans.clus$cluster, 
         color=TRUE, shade=TRUE, labels=2, lines=1)

## profiling the clusters
KRCDF$Clusters <- kmeans.clus$cluster
View(KRCDF)
aggr = aggregate(KRCDF[,-c(1,2, 8)],list(KRCDF$Clusters),mean)
clus.profile <- data.frame( Cluster=aggr[,1],
                            Freq=as.vector(table(KRCDF$Clusters)),
                            aggr[,-1])

View(clus.profile)

