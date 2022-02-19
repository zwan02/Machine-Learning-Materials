
########## Bootstrap #############

library(caret)
library(dplyr)

data("iris")
str(iris)

#### split the data

set.seed(123)
dividedata<- createDataPartition(iris$Species, p=0.75, list = FALSE)
train<- iris[dividedata,]
test<- iris[-dividedata,]




##### build the model
set.seed(123)
knnmodel<- train(Species~., method="knn", data=train, tuneLength=7)
knnmodel
knnmodel$bestTune


set.seed(123) #bootstrap is a resampling technique so we have to set seed every time 
knnmodel<- train(Species~., method="knn", data=train, 
                 tuneGrid=expand.grid(k=c(5,11,21,25))) #using odd number: to break the tie
knnmodel
knnmodel$bestTune


#### k-fold example
trainControl<- trainControl(method = "LOOCV")
set.seed(123)
LOOCVmodel<- train(Species~., method="knn", data=train, trControl=trainControl)
LOOCVmodel$bestTune
plot(LOOCVmodel)
LOOCVmodel #Resampling: Leave-One-Out Cross-Validation 
newpred<- predict(LOOCVmodel, newdata=test)
mean(newpred==test$Species)  #perfect accuracy, mean=1
confusionMatrix(newpred, test$Species)



## K-fold model
trainControl<- trainControl(method="cv", number=10) #number: number of fold
set.seed(123)
knnmodel_kfold<- train(Species~., method="knn", data=train, trControl=trainControl)
knnmodel_kfold
newpred<- predict(knnmodel_kfold, newdata=test)
mean(newpred==test$Species)
confusionMatrix(newpred, test$Species)
# knn is non-linear and non-parametric 



## lda model
trainControl<- trainControl(method="cv", number=10) #number: number of fold
set.seed(123)
ldamodel<- train(Species~., method="lda", data=train, trControl=trainControl)
ldamodel
newpred<- predict(ldamodel, newdata=test)
mean(newpred==test$Species)
confusionMatrix(newpred, test$Species)

## qda model
trainControl<- trainControl(method="cv", number=10) #number: number of fold
set.seed(123)
qdamodel<- train(Species~., method="qda", data=train, trControl=trainControl)
qdamodel
newpred<- predict(qdamodel, newdata=test)
mean(newpred==test$Species)
confusionMatrix(newpred, test$Species)

# LDA gives a lot of information even violated assumptions
# QDA might not have the information we need if it violates the assumptions





########## Bootstrap sample ##############
library(ISLR)
data("Auto")

trainControl<- trainControl(method="boot", number=100)
set.seed(1)
bootmodel<- train(mpg~horsepower, data=Auto, method="lm", trControl=trainControl)
bootmodel

#install.packages("boot")
library(boot)
bootfn<- function(data, index){
  return(coef(lm(mpg~horsepower, data = data, subset = index)))
}
bootfn(Auto, 1:392)
bootcorr<- boot(Auto, bootfn, 1000) #boot(data = Auto, statistic = bootfn, R = 1000)
summary(bootcorr) #bootstrap helps with the measure of standard error, it also helps with us assure with our results
plot(bootcorr)

summary(lm(mpg~horsepower, data=Auto))




