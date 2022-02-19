
########## Validation ###########


rm(list=ls())
############# LOOCV ###############
library(mltools)
library(ISLR)
library(caret)

data(Auto)
attach(Auto)
set.seed(1)
divideData<- createDataPartition(Auto$mpg,p=0.7,list=FALSE)
train<- Auto[divideData,]
test<- Auto[-divideData,]
trainControl<- trainControl(method = "LOOCV")

LOOCVmodel<- train(mpg~horsepower, data=train, 
                   method="lm",trControl=trainControl)
print(LOOCVmodel)
newpred<-  predict(LOOCVmodel, newdata = test)


## comparing to test data
mse<- mse(newpred,test$mpg);mse
RMSE<- rmse(newpred, test$mpg);RMSE
RSquare<- R2(newpred, test$mpg);RSquare
mae<- MAE(newpred, test$mpg);mae



########### K-fold ######################

data(Auto)
set.seed(1)
trainControl<- trainControl(method="cv", number=10)
kmodel<- train(mpg~horsepower, data=train, method="lm",
               trControl=trainControl)
print(kmodel)
newpred<- predict(kmodel, newdata = test)

## comparing to test data
mse<- mse(newpred, test$mpg); mse
RMSE<- rmse(newpred, test$mpg);RMSE
RSquare<- R2(newpred, test$mpg);RSquare
mae<- MAE(newpred, test$mpg);mae



