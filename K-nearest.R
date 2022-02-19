#library(caret)
#library(dplyr)

mower <- read.csv("LawnMowers.csv", stringsAsFactors = TRUE)
class(mower$Ownership)

### split my data set 
set.seed(123)
divideData<- createDataPartition(mower$Ownership, p=0.5, list = FALSE)
train<- mower[divideData,]
test<- mower[-divideData,]

## visualize
plot(Lot_Size~Income, data=train, pch=ifelse( train$Ownership== "Owner", 1, 3))
plot(Lot_Size~Income, data=test, pch=ifelse( test$Ownership== "Owner", 1, 3))

### make my model
knnfit <- train(Ownership~., data=train, method="knn", preProcess=c("center", "scale"))
#set seed again 

plot(knnfit)
knnfit$bestTune  #k value when it has the highest accuracy rate

## make predictions
knnclass<- predict(knnfit, newdata = test)
head(knnclass)


## calculate accuracy rate
table(knnclass, test$Ownership)
1-7/24  #this is the accuracy rate from the observations from the prediction table
7/24  #this is the error rate
mean(knnclass==test$Ownership)


## a new function to do the calculation
confusionMatrix(knnclass, test$Ownership)
# it gives you the summary of the confusion matrix, also it gives you the 
# 95% confidence interval. positive class: the type I and type II error is compared 
# with the positive class



#install.packages("mlbench")
library(mlbench)
data("PimaIndiansDiabetes2")
library(caret)
library(dplyr)
library(ggplot2)

PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)


## inspect the data
class(PimaIndiansDiabetes2$diabetes)


### visualize the data
ggplot(PimaIndiansDiabetes2, aes(glucose, mass, color=diabetes))+geom_point()


### set seed
set.seed(123)
dividedata<- createDataPartition(PimaIndiansDiabetes2$diabetes, p=0.8, list = FALSE)
train<- PimaIndiansDiabetes2[dividedata,]
test<-  PimaIndiansDiabetes2[-dividedata,]

### make the model on the trianing data 
knnfit<- train(diabetes~., data=train, method="knn", preProcess=c("center", "scale"))



## find our best # of k
knnfit$bestTune
plot(knnfit)


### make predictions
knnclass <- predict(knnfit, newdata = test)
head(knnclass)


### confusion matrix
confusionMatrix(knnclass, test$diabetes)
mean(knnclass==test$diabetes)


A<-  42; B<- 12; C<- 10; D<- 14
sensitivity<- A/(A+C); sensitivity
#how well we predicting positives

specificity<- D/ (B+D); specificity
#how well we did predicting negatives





















