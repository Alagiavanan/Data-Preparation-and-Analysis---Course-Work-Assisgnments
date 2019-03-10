#AZLAGIAVANAN SENTHIL
#A20398151
#Data Preparation and Analysis assignment 4:



#Question 1:

rm(list=ls())
library(psych)
#Loading the dataset:
wine <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
wine <- read.csv(wine,sep=',', header= F)

names(wine) <- c('Type','Alcohol','Malic_acid','Ash','Alcanity_ash','Magnesium','Total_phenols','Flavanoids','Non_flavanoids','Proanthocyanins','Color_intensity','Hue','Diluted_wines','Proline')
str(wine)

#prcomp function to perform pca on the wine data:
apply(wine,2, mean)
apply(wine, 2, var)
pr.out <- prcomp(wine, scale = TRUE)
summary(pr.out)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
dim(pr.out$x)
#Since mostly all the variables have vastly different variances, if we fail to scale the variables before pca, then most of the principal components will be driven by one particular variable which has the largest mean and variance.
#Hence it is important to standardize the variables to mean zero and sd one before performing pca.So Scaling should be done for our dataset.

#Plotting a biplot:
biplot(pr.out, scale=0)
#The feature opposite to Hue is Malic acid.Since this festure/variable are far from Hue (ie) in the opposite direction they are less correlated with Hue.
#Support for the calculated value:
pairs.panels(wine[,c(3,12)])
#The above plot shows that they are weakly correlated.The correlated value is -0.56. The correletion can also be found using correlation function in R.

#Plotting screeplot and determining the percentage of total variance explained by PC1 and PC2.
screeplot(pr.out)
pr.out$sdev
pr.var <- pr.out$sdev^2
pr.var

pve <- pr.var/sum(pr.var)
pve
plot(pve, xlab="Principal Component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
#Hence PC1 explains 39.5% of the variance and PC2 explains 17.8% of the variance and so on.


#Question 2:

rm(list=ls())
library(cluster)
library(factoextra)
library(tidyverse)

data <- USArrests

states <- row.names(data)
states

names(data)
apply(USArrests,2,mean)
apply(USArrests,2,var)

#Since mostly all the variables have vastly different variances and means, it is important to standardize the variables to mean zero and sd one.So Scaling should be done for our dataset.
#Hence scale the data before applying kmeans to it.
sd.data <- scale(data)

set.seed(2)
true.labels <- c(rep(1, 20), rep(2, 15), rep(3, 15))

#kmeans for k value from 2 to 10 with plotting of within cluster sum of squares for each k value:
km.out <- kmeans(sd.data, 2, nstart = 20)
table(true.labels, km.out$cluster)
km.out$tot.withinss
km.out$withinss
plot(km.out$tot.withinss)
plot(km.out$withinss)

km.out <- kmeans(sd.data, 3, nstart = 20)
table(true.labels, km.out$cluster)
km.out$tot.withinss
km.out$withinss
plot(km.out$tot.withinss)
plot(km.out$withinss)

km.out <- kmeans(sd.data, 4, nstart = 20)
table(true.labels, km.out$cluster)
km.out$tot.withinss
km.out$withinss
plot(km.out$tot.withinss)
plot(km.out$withinss)

km.out <- kmeans(sd.data, 5, nstart = 20)
table(true.labels, km.out$cluster)
km.out$tot.withinss
km.out$withinss
plot(km.out$tot.withinss)
plot(km.out$withinss)

km.out <- kmeans(sd.data, 6, nstart = 20)
table(true.labels, km.out$cluster)
km.out$tot.withinss
km.out$withinss
plot(km.out$tot.withinss)
plot(km.out$withinss)

km.out <- kmeans(sd.data, 7, nstart = 20)
table(true.labels, km.out$cluster)
km.out$tot.withinss
km.out$withinss
plot(km.out$tot.withinss)
plot(km.out$withinss)

km.out <- kmeans(sd.data, 8, nstart = 20)
table(true.labels, km.out$cluster)
km.out$tot.withinss
km.out$withinss
plot(km.out$tot.withinss)
plot(km.out$withinss)

km.out <- kmeans(sd.data, 9, nstart = 20)
table(true.labels, km.out$cluster)
km.out$tot.withinss
km.out$withinss
plot(km.out$tot.withinss)
plot(km.out$withinss)

km.out <- kmeans(sd.data, 10, nstart = 20)
table(true.labels, km.out$cluster)
km.out$tot.withinss
km.out$withinss
plot(km.out$tot.withinss)
plot(km.out$withinss)

#Methods to find the optimal number of clusters:
fviz_nbclust(sd.data,kmeans,method = "silhouette")
fviz_nbclust(sd.data,kmeans,method = "wss")

#Plotting the optimal cluster:
#By silhouette method:
k<-kmeans(sd.data,centers=2,nstart=20)

#Plotting of optimal cluster using kmeans by silhouette method:
fviz_cluster(k,data=sd.data)

#By WSS method:
k <- kmeans(sd.data,centers=4,nstart=20)

#Plotting of optimal cluster using kmeans by silhouette method:
fviz_cluster(k,data=sd.data)
#hence the optimum cluster would be 2 or 4 depending on the method of finding the optimal cluster.


#Question 3:


rm(list=ls())
library(cluster)
library(factoextra)
library(dplyr)
#Loading the dataset:
wine <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
wine <- read.csv(wine,sep=';', header= TRUE)
str(wine)

apply(wine,2, mean)
apply(wine, 2, var)
#Since mostly all the variables have vastly different variances and means, it is important to standardize the variables to mean zero and sd one.So Scaling should be done for our dataset.
sd.data <- scale(wine[,1:11])

#Hierarchical Clustering using complete linkage
hc.complete <- hclust(dist(sd.data), method="complete")
hc.complete
plot(hc.complete, main="Complete Linkage", xlab="",sub="",cex=.0001)
hc.complete$height
rect.hclust(hc.complete,k=2,border="red")

#Hierarchical Clustering using single linkage
hc.single<- hclust(dist(sd.data), method="single")
hc.single
plot(hc.single, main="Single Linkage",cex=.06)
rect.hclust(hc.single,k=2,border="red")

tail(hc.single$height,n=1)
tail(hc.complete$height,n=1)
#From the above results we can see that the penultimate clusters are merged at a height of 14.25 and 27.73 for single and complete linkage.

#Cutree method for complete linkage
hc_complete_cut<- cutree(hc.complete,2)
table(hc_complete_cut)

#Cutree method for single linkage
hc_single_cut<- cutree(hc.single,2)
table(hc_single_cut)

#Summary statistics for single linkage
wine$Clusters <- hc_single_cut
wine <- group_by(wine,Clusters)
hc_single <- summarise_each(wine, funs(mean))
print.data.frame(hc_single)
summary(hc_single_cut)

#Summary statistics for complete linkage
wine$Clusters <- hc_complete_cut
wine <- group_by(wine,Clusters)
hc_complete <- summarise_each(wine, funs(mean))
print.data.frame(hc_complete)
summary(hc_complete_cut)

#Residual.sugar feature has the largest cluster mean difference as observed from the above results.

#From the above results and Dendogram graphs we can say that Complete Linkage method dendogram is much more balanced than the Single Linkage Dendogram.











