#AZLAGIAVANAN SENTHIL
#A20398151
#Data Preparation and Analysis assignment 2:


#Question 1:
library(MASS)
library(ISLR)
library(ggplot2)
data <- Boston
summary(data)
lrfit <- lm(medv~lstat, data)
summary(lrfit)
plot(medv~lstat, data)
abline(lrfit,col = "darkgreen")
lrresidual <- resid(lrfit)
plot(predict(lrfit), residuals(lrfit))
plot(data$medv,lrresidual,ylab="RESIDUALS",xlab = "FITTED VALUES", main="Fitted vs Residual plot")
abline(0,0,col="red")
predict(lrfit, data.frame(lstat = c(5,10,15)), interval = "confidence")
predict(lrfit, data.frame(lstat = c(5,10,15)), interval = "prediction")
data$lstat2 <- data$lstat^2
mrfit <- lm(medv ~ lstat + lstat2, data)

summary(lrfit)$r.squared
summary(mrfit)$r.squared



ggplot(data = data, aes(x = lstat, y = medv)) + 
  geom_point() + 
  theme_bw() + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 1))

ggplot(data = data, aes(x = lstat2, y = medv)) + 
  geom_point() + 
  theme_bw() + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 1))

ggplot(data = data, aes(x = lstat+lstat2, y = medv)) + 
  geom_point() + 
  theme_bw() + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 1))

ggplot(data = data, aes(x = lstat, y = medv)) + 
  geom_point() + 
  theme_bw() + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 2))

ggplot(data = data, aes(x = lstat2, y = medv)) + 
  geom_point() + 
  theme_bw() + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 2))

ggplot(data = data, aes(x = lstat+lstat2, y = medv)) + 
  geom_point() + 
  theme_bw() + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 2))





#Question 2:
rm(list=ls())
library(rpart)
library(caret)
library(rpart.plot)
library(ROCR)
library(corrplot)
#dataset from UCI:
abalone<-read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"),header=FALSE,as.is=TRUE,sep=',')
View(abalone)
abalone$V1 <- as.factor(abalone$V1)
str(abalone)

#removing of Infant category from V1:
abalone <- abalone[abalone$V1 != "I", ]
abalone$V1 <- factor(abalone$V1, labels = c('M','F'))
str(abalone$V1)
#Creating partition using createDataPartition method:
index <- createDataPartition(abalone$V1, p = 0.8, list = F)
train <- abalone[index, ]
test <- abalone[-index, ]
#Building a model using glm function:
model <- glm(V1 ~., data = train, family = binomial)
summary(model)
confint(model)
confint(model,  level = 0.95, trace = FALSE)



#Building a confusion matrix using 50% cutoff value to Male/Female:
resp <- predict(model, test,type="response")
pred <- ifelse(resp > 0.5, "F", "M") 
confusionMatrix(pred, test$V1)

#Roc and auc for the model:
pred.rocr <- predict(model, newdata=test, type="response")
f.pred <- prediction(pred.rocr, test$V1)
f.perf <- performance(f.pred, "tpr", "fpr")
plot(f.perf, colorize=T, lwd=3)
abline(0,1)
auc <- performance(f.pred, measure = "auc")
auc@y.values[[1]]


#correlation plot for the numerical variables in the dataset:
cop = cor(abalone[2:9])
corrplot(cop)
corrplot(cop, method="number")

#new model built after obtaining the correlation plot:
model1 <- glm(V1~V3+V6, data = train, family = "binomial")
summary(model1)
resp <- predict(model1, test,type="response")
pred <- ifelse(resp > 0.5, "F", "M") 
confusionMatrix(pred, test$V1)

pred.rocr <- predict(model1, newdata=test, type="response")
f.pred <- prediction(pred.rocr, test$V1)
f.perf <- performance(f.pred, "tpr", "fpr")
plot(f.perf, colorize=T, lwd=3)
abline(0,1)
auc <- performance(f.pred, measure = "auc")
auc@y.values[[1]]

important_features <- varImp(model, scale = FALSE)
print(important_features)


#Question3:
rm(list=ls())
library(rpart)
library(caret)
library(rpart.plot)
library(ROCR)
library(e1071)


mushroom <- "https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"
mushroom <- read.csv(mushroom,sep = ",",header = F)
names(mushroom) <- c('label','Cap_shape','cap_surface','cap_color','bruises','odor','gill_attachment','gill_spacing','gill_size','gill_color','stalk_shape','stalk_root','stalk_surface_above_ring','stalk_surface_below_ring','stalk_color_above_ring','stalk_color_below_ring','veil_type','veil_color','ring_number','ring_type','spore_print_color','population','habitat')
head(mushroom)

set.seed(1122)

index<- sample(1:nrow(mushroom), size=0.2*nrow(mushroom))
test <- mushroom[index, ]
train <- mushroom[-index, ]

model<- naiveBayes(x=train[-1],y=train$label)
summary(model)
pred <- predict(model, newdata = test[-1])


table(pred,test$label)
confusionMatrix(pred,test$label)


#number of false positives observed is 78 and the accuracy of the model prediction is 0.9483
# Positive class is edible mushrooms.
# Accuracy for the classifier on the train and the test dataset almost the same (0.94).

# removing rows where ? is present
set.seed(1122)

index1 <- which(mushroom$stalk_root == "?")
mushroom_new <- mushroom[-index1,]


index1 <- sample(1:nrow(mushroom_new), size=0.2*nrow(mushroom_new))
test_new <- mushroom_new[index1, ]
train_new <- mushroom_new[-index1, ]

model1<- naiveBayes(x=train_new[-1],y=train_new$label)
pred1<- predict(model1, newdata = test_new[-1])

confusionMatrix(pred1,test_new$label)


# Accuracy of the classifier after the removal of the rows containing ? is slightly (1%) higher than the classifier containing all the observations.  

