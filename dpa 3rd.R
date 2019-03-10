#Azlagiavanan Senthil
#A20398151
#DPA 3rd assignment

#Question 1: 

rm(list=ls())
library(caret)
yacht <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00243/yacht_hydrodynamics.data"
yacht <- read.csv(yacht,sep = "",header = F)
head(yacht)
names(yacht) <- c('Longitudinal_position','Prismatic_coefficient','Length_displacement_ratio','Beam_draught_ratio','Length_beam_ratio','Froude_number','Residuary_resistance')
str(yacht)


set.seed(1)
index <- createDataPartition(yacht$Residuary_resistance, p = 0.8,list = F)
train <- yacht[index,]
test <- yacht[-index,]

# Linear model

fit <- lm(Residuary_resistance~.,data=train)
summary(fit)
summary.fit<-summary(fit)

plot(fit,1)
fit.predict <- predict(fit,test, interval="prediction")

#MSE for the train data
MSE_train <- function(error){mean(error^2)}
MSE_train(fit$residuals)

#RMSE for the train data
RMSE_train <- function(error){sqrt(mean(error^2))}
RMSE_train(fit$residuals)

#R squared value:
summary(fit)$r.squared

#MSE for the test data:
MSE_test <- mean(test$Residuary_resistance-predict.lm(fit,test))^2
MSE_test

#RMSE for the test data
RMSE_test <- sqrt(MSE_test)
RMSE_test

#Bootstrap using train control method:
tc <- trainControl(method="boot", number=1000)
tc

#Bootstrap using glm boost:
model <- train(Residuary_resistance~.,data=train,trControl=tc,method="glmboost")
model

#histograms of the RMSE values:
model$results$RMSE
model$results$Rsquared
histogram(model$results$RMSE,xlab="RMSE")

#Finding the mean of the RMSE and Rsquared:
mean(model$results$RMSE)
mean(model$results$Rsquared)

## Linear model comparison
sqrt(mean(summary.fit$residuals^2))


Activity_test <- predict(model, newdata = test)

res_yhd_bs <- (Activity_test-test$Residuary_resistance)
mse_yhd_bs <- mean(res_yhd_bs^2)
mse_yhd_bs

RSS_v_boot <-sum(res_yhd_bs^2)
TSS_v_boot <-sum((test$Residuary_resistance-mean(test$Residuary_resistance))^2)
r.squared <- (1-(RSS_v_boot/TSS_v_boot))
r.squared

#Comparing the values we can see that the normal linear model performs better when compared to the bootstraped model because the linear model had higher R squared values and low MSE when compared that to that of the Bootstraped model.

#Question 2:

rm(list=ls())

library(plyr)
library(glmnet)
library(caret)
library(ModelMetrics)
library(qpcR)
german <- "https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data-numeric"
german <-  read.csv(german,sep='', header= F)

german$V25 <- as.factor(german$V25)
german$V25
revalue(german$V25, c('2'='0'))
View(german)


set.seed(100)
index <- createDataPartition(german$V25, p = 0.8, list = F)
train<- german[index,]
test<- german[-index,]

# Linear model
model <- glm(V25 ~., data = train, family=binomial)
plot(model)
print(model)
summary(model)

#MSE for training data
MSE_train <- function(error){mean(error^2)}
MSE_train(model$residuals)

#RMSE for training data
RMSE_train <- function(error){sqrt(mean(error^2))}
RMSE_train(model$residuals)

#R squared  value:
nullmod <- glm(V25~1, family=binomial, data = train)
Rsquared_value <- 1-logLik(model)/logLik(nullmod)
Rsquared_value

#Cross fold validation method:
ctrl <- trainControl(method = "repeatedcv",number=10,repeats = 5)
glm1_cv <- train(V25~., data = train, method = "glm",
                 trControl = ctrl)

summary(glm1_cv)

Activity_test <- predict(glm1_cv, newdata=test)
Activity_train <- predict(glm1_cv, newdata=train)

#RMSE for the training and testing model of cv method:
RMSE_cv_train <- sqrt(mean((as.integer(train$V25)-(as.integer(Activity_train)))^2))
RMSE_cv_train
RMSE_cv_test <- sqrt(mean((as.integer(test$V25)-(as.integer(Activity_test)))^2))
RMSE_cv_test

#MSE for the training and testing model of cv method:
MSE_cv_train <- (mean((as.integer(train$V25)-(as.integer(Activity_train)))^2))
MSE_cv_train
MSE_cv_test <- (mean((as.integer(test$V25)-(as.integer(Activity_test)))^2))
MSE_cv_test

#R squared value:

#The MSE,RMSE and R squared for the linear model is much better than the crossfold model. Hence crossfold model doesnt perform well when compared to that of the linear model.

#Question 3:

rm(list=ls())

#Loading libraries:
library(glmnet)
library(caret)

#Loading dataset :
cars <- mtcars

#Setting up a model matrix:
x  <- model.matrix(mpg~.,cars)[,-1]
y <- cars$mpg

#80-20 split on the dataset:
set.seed(100)
index <- createDataPartition(cars$mpg, p = 0.8,list = F)
train<-cars[index,]
test <- cars[-index,]

#Dummy variable for variable am:
cars$am <- factor(cars$am)

#Linear regression model on the dataset:
lm_mtcars <- lm(mpg~.,data=train)
summary(lm_mtcars)
# Based on the summary of the linear model we can observe that wt,am and hp have high T-value corresponding to other predictors.

#Coefficients of the predictors:
coef(lm_mtcars)

#Tuning lambda and using cross-validation to determine the minimum value of lambda:
lambda <- 10^seq(10,-2,length=100)
cv.out <- cv.glmnet(x[index,],y[index],alpha=0)
bestlam <- cv.out$lambda.min
bestlam
#Plotting MSE as a function of lambda:
plot(cv.out)

#Building a ridge model and finding the Out of sample MSE using predict function 
ridge.mod <- glmnet(x[index,],y[index],alpha=0,lambda= lambda,thresh=1e-12)
plot(ridge.mod)
ridge.predict <- predict(ridge.mod,s=bestlam,newx=x[-index,])

#Checking MSE for the lasso model:
mean((ridge.predict-test[,1])^2)

#Coefficients for the ridge model:
ridge.coef <- predict(ridge.mod,type="coefficients",s=bestlam)

ridge.coef

#The Coefficients obtained using Ridge Regression is slightly different to Coefficients obtained from the Linear Regression model.
#Hence ridge regression performs only shrinkage and not variable selection. Ridge regression includes all of the variables in the final model.


#Question 4:

rm(list=ls())

#Loading libraries:
library(glmnet)
library(caret)

#Loading dataset
swiss <- swiss

#Setting up a model matrix:
x <- model.matrix(Fertility~., swiss)[,-1]
y <- swiss$Fertility

#80-20 split on the dataset:
set.seed(489)
train = sample(1:nrow(x), 0.8*nrow(x))
test = (-train)
ytest = y[test]

#Linear regression model on the dataset:
swisslm <- lm(Fertility~., data = swiss)
summary(swisslm)
# Based on the summary of the linear model,we can observe that Education,Catholic and Infant.Mortality have high T-value corresponding to other predictors.
#Coefficient of the predictors:
coef(swisslm)

#Tuning lambda and using cross-validation to determine the minimum value of lambda:
lambda <- 10^seq(10, -2, length = 100)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
bestlam <- cv.out$lambda.min
bestlam
#Plotting MSE as a function of lambda:
plot(cv.out)


#Building a lasso model and finding the Out of sample MSE using predict function 
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = lambda)
plot(lasso.mod)
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])

#Checking MSE for the lasso model:
mean((lasso.pred-ytest)^2)


#Coefficients for the lasso model:
lasso.coef  <- predict(lasso.mod, type = 'coefficients', s = bestlam)[1:6,]

lasso.coef

# The coefficients obtained using lasso Regression is similar to Coefficients obtained from the Linear Regression model with slight deviation.

#Hence lasso regression has performed both shrinkage and variable selection.Lasso regression has improved prediction error by shrinking the regression coefficients and variable selection also because we found that examination was not an important predictor when we did linear regression but after lasso we get examination as a good predictor and not Catholic since it brings the model close to zero.


