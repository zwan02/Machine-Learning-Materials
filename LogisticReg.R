rm(list=ls())
library(ISLR)
data("Default")
str(Default)
library(ggplot2)


ggplot(Default, aes(balance, income, group=default))+geom_point(aes(shape=default,color=default))
#see the graph to have a first glance of the data

#install.packages("caret") - for classification and regression training
library(caret)
set.seed(1)
divideData <- createDataPartition(Default$default,p=0.8, list = F)
train<-  Default[divideData,]
test<-  Default[-divideData,]


logisticreg<- glm(default~balance,family=binomial, data=train)
options(scipen = 999) #we can see the data better 
summary(logisticreg)

#moves log odds to odds
exp(coef(logisticreg)) #we use exp function to put the coefficient into the equation
#and this is easier to interpret

#move odds to probability
b0 <- logisticreg$coefficients[1]; b0
b1 <- logisticreg$coefficients[2]; b1

probdefault <- exp(b0+b1)/(1+exp(b0 + b1)); probdefault # this is for p-hat

log(probdefault/(1-probdefault)) #log odds or the logit transformation of p(X)
# equals to the sum of the logistic regression coefficients


b0+b1

#Predicted Probabilities using logistic regression
library(tidyverse)
prob <- ifelse(Default$default == "Yes", 1, 0)

ggplot(Default, aes(balance, prob)) + geom_point(alpha = .1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("Balance") + ylab("Probability of Default")


#What if a customer has a balance of 1500?
x<-1500 #setting the value for balance
predict(logisticreg, data.frame(balance=x), type="response")

#predict function: plugging in the values in p-hat=exp(b0+b1*x)/1+exp(b0+b1*x)
exp(b0+b1*x)/(1+exp(b0+b1*x))

##############################################
### calculate the testing error and accuracy rate
probs<- predict(logisticreg,test,type="response")
pred<- ifelse(probs>0.5, "Yes", "No")
table(pred, test$default)

mean(pred!=test$default)  #test error rate =  0.02901451
(6+52)/1999
mean(pred==test$default)  #test accuracy rate = 0.9709855
#1-test error rate

#####################################
#### Training accuracy and error rate
probs <- predict(logisticreg,train, type = "response")
pred<- ifelse(probs>0.5, "Yes","No")
table(pred, train$default)
mean(pred!=train$default) #training error rate = 0.02712161
mean(pred==train$default) #training accuracy rate = 0.9728784
#1-train error rate

########################################
#### multiple logistic regression example
multilog<- glm(default~student + balance, family=binomial, data=train)
summary(multilog)

exp(multilog$coefficients) #using exp to appropriate interpret the coefficients

probs<-  predict(multilog, test, type="response")
pred<-  ifelse(probs>0.5, "Yes", "No")
table(pred, test$default)

mean(pred!=test $default)  ##test error rate = 0.02751376
mean(pred==test$default) ## test accuracy rate = 0.9724862
#the goal is to get as many accuracy as possible 

#####################################################
#### assumption 1 : linearity with the logic  ######
attach(Default)
plot(balance, log(balance)) 
#the plot doesn't look linear and it violates the assumption of linearity

interaction<- balance*log(balance)
checkinterct<- glm(default~interaction, family=binomial, data=Default)
summary(checkinterct)
# by looking at the summary, every variables are significant and it violates the 
#assumption of linearity


#####################################################
### assumption 2: Absence of multicollinearity #####
car::vif(multilog)
#all under 5, which is good


################################################################
#####  assumption 3: Lack of Strongly Influential Outliers ###### 
library(broom)
library(tidyverse)

modelResults <- augment(multilog) %>% mutate(index = 1:n())
ggplot(modelResults, aes(index, .std.resid))+geom_point(aes(color=default))
ggplot(modelResults, aes(index, .cooksd))+geom_point(aes(color=default))
## the ggplots provides two different ways for us to see the results

###independent errors - check for no repeated errors 






##############################################################
########### Example 2: Breast Cancer #######################
#############################################################
#install.packages('mlbench')
library(mlbench)
data("BreastCancer")
str(BreastCancer)

#get rid of the id variable
BreastCancer <- BreastCancer[,-1]
BreastCancer <- na.omit(BreastCancer) #get rid of NA values

set.seed(123)
#we are predicting class variable in the data set
divideData <- createDataPartition(BreastCancer$Class, p=0.7, list = F)

#separate training and testing data
train <- BreastCancer[divideData, ]
test  <- BreastCancer[-divideData, ]


#build the model by the training data
model<- glm(Class~., data = train, family = binomial)
summary(model)



## test accuracy and error rate
prob <- predict(model, test, type="response")
pred <- ifelse(prob>0.5, "malignant", "benign")

#check the result for the prediction from our testing dataset
table(pred, test$Class)

mean(pred==test$Class) #accuracy rate = 0.9264706
mean(pred!=test$Class) #error rate = 0.07352941



















