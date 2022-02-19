rm(list=ls())
#install.packages("rattle")
library(rattle)

data(wine)
str(wine)
attach(wine)

library(MASS) #it helps the LDA function
wine.lda<- lda(Type~.,data = wine) #to build the linear discriminant model of Type
wine.lda #no summary function
#things to look at: Prior probabilities of groups, Group means - find the group differences
#Coefficients of linear discriminant - make up of equations

####what does the LD1/LD2 represents? (looking by columns in data)
# the difference of observation probabilities of the 3 groups 

#### linear discrimination function -- LD1 -0.403399781*Alcohol+0.165254596*Malic+...


#### making predictions
wine.lda.values <- predict(wine.lda)
wine.lda.values
#taking the 178 probability observations and find the highest one from every observation

### create histogram
ldahist(data=wine.lda.values$x[,1], g=Type) #LD1 histogram
ldahist(data=wine.lda.values$x[,2], g=Type) #LD2 histogram


library(ggplot2)
ggplot(wine, aes(wine.lda.values$x[,1],wine.lda.values$x[,2]))+geom_point(aes(color=Type))


### split data to find the accuracy rate
library(caret)
library(tidyverse)
set.seed(1)
divideData <- Type %>% createDataPartition(p=0.8, list = F)
train<- wine[divideData,]
test<- wine[-divideData,]


### centering and scaling 
preprocessing<- train %>%  preProcess(method=c("center", "scale"))
traintransformed <-  preprocessing %>% predict(train)
testtrainsformed <- preprocessing %>% predict(test)


#### make our model by using the trained transformed dataset
model <- lda(Type~., data = traintransformed)
plot(model)


##### make predictions by using our model of trained data
predictions <- model %>% predict(testtrainsformed)
names(predictions)
mean(predictions$class==testtrainsformed$Type)
table(predictions$class, testtrainsformed$Type)
###################################what does the mean=1 mean?
#100% accuracy this is good

###
ldaforgraph<- cbind(traintransformed, predict(model)$x) 
ggplot(ldaforgraph, aes(LD1, LD2))+geom_point(aes(color=Type))


###no center and scale             
model<- lda(Type~., data = train)
predictions <- model %>% predict(test)
mean(predictions$class==test$Type)


#################  what's the difference of center and scaling and no center and scaling?/ what is this for?
# center and scale usually helps, easy to compare cross the columns 
#since they have the same scaling standard, helps with accuracy





############### from lda to qda ####################

#moving from lda to qda is to improve the accuracy and fine the better model
#qda requires more sample size and more variables 

library(rattle)
#pull out the wine database 

library(MASS)
library(tidyverse)
library(caret)

attach(wine)
##### separate the data
set.seed(1000)
divideData <- Type %>% createDataPartition(p=0.35, list = FALSE)
# we need the list to be False, because we can separate as training and testing groups 
train<-  wine[divideData,]
test<-  wine[-divideData,]

#centering and scale
preprocessing <- train %>% preProcess(method=c("center","scale"))
traintransformed<- preprocessing %>% predict(train)
testtrainsformed<- preprocessing %>% predict(test)


##### make a model - qda
model<- qda(Type~.,data = traintransformed)
model

#### make predictions
predictions <- model %>% predict(testtrainsformed)
mean(predictions$class==testtrainsformed$Type) #1 means perfect accuracy




#################################################
##################### Another example ##########
################################################

phoneme<- read.csv("phoneme.csv",stringsAsFactors = TRUE)
phoneme <- phoneme[,-c(1,259)]
str(phoneme)
class(phoneme$g)

attach(phoneme)


#### divide our data
set.seed(100)
divideData<- createDataPartition(g,p=0.4, list = FALSE)
train<- phoneme [divideData,]
test<- phoneme[-divideData,]


### centering and scale
preprocessing <- train %>% preProcess(method=c("center","scale"))
traintransformed <- preprocessing %>% predict(train)
testtrainsformed<- preprocessing %>% predict(test)


### make a lda model
ldamodel<- lda(g~., data=traintransformed)
ldamodel


## calculate accuracy rate and error rate (1-accuracy rate)
predictions<- ldamodel %>% predict(testtrainsformed)
mean(predictions$class==testtrainsformed$g)
# given a proportion of 0.2 and a set seed of 100, our accuracy rate is 0.9098474
# given a proportion of 0.8 and a set seed of 100, our accuracy rate is 0.9344444
table(predictions$class, testtrainsformed$g)

## visualize - graph this data
library(ggplot2)
ldaforgraph<- cbind(traintransformed,predict(ldamodel)$x)

#4 LDs for 5 sounds
ggplot(ldaforgraph, aes(LD1, LD2))+geom_point(aes(color=g,shape=g))
ggplot(ldaforgraph, aes(LD1, LD3))+geom_point(aes(color=g,shape=g))
ggplot(ldaforgraph, aes(LD1, LD4))+geom_point(aes(color=g,shape=g))
ggplot(ldaforgraph, aes(LD2, LD3))+geom_point(aes(color=g,shape=g))
ggplot(ldaforgraph, aes(LD2, LD4))+geom_point(aes(color=g,shape=g))
ggplot(ldaforgraph, aes(LD3, LD4))+geom_point(aes(color=g,shape=g))


# qda
qdamodel<- qda(g~.,data=traintransformed)
qdamodel

# make predicitons and calculate accuracy rate
predictions<- qdamodel%>% predict(testtrainsformed)
mean(predictions$class==testtrainsformed$g)
# given a proportion of 0.8 and a set seed of 100, our accuracy rate is 0.8622222
# the reason of why the result is worse is overfit 

table(predictions$class, testtrainsformed$g)




























