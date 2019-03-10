#Azlagiavanan Senthil
#A20398151
#Data Preparation and Analysis assignment 1


rm(list=ls())

library(datasets)
library(ggplot2)
library(moments)

#Loading iris dataset to a dataframe
data<-iris
summary(iris)
View(data)
#boxplot for each feature
boxplot(data$Sepal.Length)
boxplot(data$Sepal.Width)
boxplot(data$Petal.Length)
boxplot(data$Petal.Width)

#calculating IQR for each feature
IQR(data$Sepal.Length)
IQR(data$Sepal.Width)
IQR(data$Petal.Length)
IQR(data$Petal.Width)
#Petal.Length has the largest IQR value

# calculating Standard deviation for each feature
sd(data$Sepal.Length)
sd(data$Sepal.Width)
sd(data$Petal.Length)
sd(data$Petal.Width)

#The sd values and IQR values does not agree with each other. IQR measures variability only within the 50% of the dataset whereas sd is the most robust measure which takes into account how every value in a dataset varies from the mean.
#hence considering this sd values should be taken into consideration since the order of measure is not the same for sd and IQR.



#Using ggplot2 to create colored boxplot for each feature.
boxplot(Sepal.Length ~ Species, data=iris,main="Boxplot for Sepal length using the species class",col=c("pink","green","blue"))
boxplot(Sepal.Width ~ Species, data=iris,main="Boxplot for Sepal width using the species class",col=c("pink","green","blue"))
boxplot(Petal.Length ~ Species, data=iris,main="Boxplot for Petal length using the species class",col=c("pink","green","blue"))
boxplot(Petal.Width ~ Species, data=iris,main="Boxplot for petal width using the species class",col=c("pink","green","blue"))

#Setosa has the significantly different petal length or width once it is separated from the other classes

#Question2:
library(datasets)
#Loading trees dataset to a dataframe
data1<-trees
#summary of the dataset
summary(data1)
View(data1)
#Summary of each feature
summary(data1$Girth)
summary(data1$Height)
summary(data1$Volume)
#Histogram of each variable
hist(data1$Girth,freq=F)
lines(density(data1$Girth))
hist(data1$Height,freq=F)
lines(density(data1$Height))
hist(data1$Volume,freq=F)
lines(density(data1$Volume))
#Histograms in a single window
par(mfrow=c(2,2))
hist(data1$Girth,freq=F)
lines(density(data1$Girth))
hist(data1$Height,freq=F)
lines(density(data1$Height))
hist(data1$Volume,freq=F)
lines(density(data1$Volume))


mean(data1$Girth)
median(data1$Girth)
mean(data1$Height)
median(data1$Height)
#No feature/variable is normally distributed.
#The features Volume and Girth has positive skewness.
#The feature Height has negative skewness.

skewness(data1$Girth)
skewness(data1$Height)
skewness(data1$Volume)

#Yes the values agree with visual inspection.



#Question3:
#Loading the auto.mpgg dataset to a dataframe
auto<-read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data"),header=FALSE,as.is=TRUE,sep='')
View(auto)
#summary of the dataset
summary(auto)

#in auto1 NA values are replaced with median and auto2 NA values are omitted
auto1<-auto
auto2<-auto

#Using as.numeric casting funstion and replacing NA values with median
auto1$V4 = as.numeric(auto1$V4)
is.na(auto1$V4)
auto1$V4[which(is.na(auto1$V4))]<-median(auto1$V4,na.rm = TRUE)
View(auto1)

#Omitting NA values
auto2$V4 = as.numeric(auto2$V4)
is.na(auto2$V4)
auto2<-na.omit(auto2)
View(auto2)

summary(auto1)
summary(auto2)
#Replacing NA with median just makes a very small difference, hence it doesnt affect a lot.



