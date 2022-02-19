###Team 22 Case 4
### Fares Sukkar, Spencer Tsai, Frank Wan, Zach Washburn

rm(list = ls())

### Load libraries
library(ISLR)
library(ggplot2)
library(caret)
library(broom)
library(tidyverse)
library(MASS)
library(rattle)
library(car)

Case4 <- read.csv("Case4.csv", stringsAsFactors = TRUE)
attach(Case4)

### Divide data
set.seed(123)
divideData <- createDataPartition(intubated, p = 0.30, list = F)
train <- Case4[divideData,]
test <- Case4[-divideData,]

###### LOG: Age and Gender ######

### Build model 
ShortLogModel <- glm(intubated~age+Gender, family = "binomial", data = train)
options(scipen = 999)
summary(ShortLogModel)
exp(ShortLogModel$coefficients)

# (Intercept) age         GenderWoman 
# 0.1256891   1.0128787   0.7624766 

# According to the coefficients of age and GenderWoman,  a one unit increase in age
# causes the odds for being intubated to increase by a factor of 1.0129. 
# The odds for being intubated are 23.8% (1-0.7624766) lower for women.

### Testing rate
probsShort <- predict(ShortLogModel, test, type = "response")
predShort <- ifelse(probsShort > 0.5, "Yes","No")
table(predShort,test$intubated)
mean(predShort!=test$intubated)
# Test error rate: 0.1819232
mean(predShort==test$intubated)
# Test accuracy rate: 0.8180768

# We predicted that people do not need to be intubated correctly 81.81% of the
# time. Oppositely, we falsely predicted that people need to be intubated 
# 18.19% of the time.

# predShort    No    Yes
#          No 36105  8029

# We classified incorrectly 18.19% of the time. We correctly predicted 36105 "No", 
# thus the test accuracy rate is 81.81%. (36105) / (44134). We falsely predicted 
# 8029 "No", thus the test error rate is 18.19%. (8029) / (44134).

### Check assumptions

## 1. Linearity with Logit
plot(age,log(age))
interaction <- age*log(age)
interact <- glm(intubated~interaction, family = "binomial", data = Case4)
summary(interact)
# Plot does not appear linear, and the terms are significant, so assumption of 
# linearity of the logit is violated.

## 2. Multicollinearity
vif(ShortLogModel)
# All scores are below 5, indicating no multicollinearity.

## 3. Outliers
modelResults <- augment(ShortLogModel) %>% mutate(index=1:n())
ggplot(modelResults, aes(index, .std.resid)) + 
  geom_point(aes(color = intubated))
ggplot(modelResults, aes(index, .cooksd)) + geom_point(aes(color = intubated))
# No strong, influential outliers present in either of the plots.


###### LOG: All predictors ######

### Make model
LogisticModel <- glm(intubated~., family = "binomial", data = train)
summary(LogisticModel)
exp(LogisticModel$coefficients)

### Make predictions and assess
probsLog <- predict(LogisticModel, test, type = "response")
predLog <- ifelse(probsLog > 0.5, "Yes","No")
table(predLog,test$intubated)
mean(predLog!=test$intubated)
# Test error rate: 0.1819459
mean(predLog==test$intubated)
# Test accuracy rate: 0.8180541

# predLog       No   Yes
#         No  36101  8026
#        Yes     4     3

# We classified correctly 81.81% of time. We classified incorrectly 18.19% of
# the time. We correctly predicted 36101 "No" and 3 "Yes", thus the test
# accuracy rate is 81.81%. (36101+3) / (44134). We falsely predicted 4 "Yes" and
# 8026 "No", thus the test error rate is 18.19%. (8026 + 4) / (44134).


###### LDA: All predictors ######

### Transform data
preProcessing <- train %>% preProcess(method = c("center","scale"))
traintransform <- preProcessing %>% predict(train)
testtransform <- preProcessing %>% predict(test)

### Build model
ldaModel <- lda(intubated~., data = traintransform)
# Warning message indicating that variables are collinear

### Predictions
predLDA <- ldaModel %>% predict(testtransform)
mean(predLDA$class!=testtransform$intubated)
# 0.1820139 error rate
mean(predLDA$class==testtransform$intubated)
# 0.8179861 accuracy rate
table(predLDA$class,testtransform$intubated)

#         No   Yes
#  No  36098  8026
#  Yes     7     3

# We classified correctly 81.80% of the time.
# We classified incorrectly 18.20% of the time. We correctly predicted 36098 "No" 
# and 3 "Yes", thus the test accuracy rate is 81.80%. (36098+3) / (44134). We 
# falsely predicted 7 "Yes" and 8026 "No", thus the test error rate is 18.20%. 
# (8026 + 7) / (44134).


###### QDA: All predictors ######

### Make model
qdaModel <- qda(intubated~.,data = traintransform)

# We were not able to run the QDA model. We received an error regarding rank
# deficiency, which means that some variables are collinear.
# We ran the QDA model with just gender and age; however, we were unable to
# run the model with all of the predictors included. This could be due to 
# strongly correlated data elements. 

# Another potential cause of rank deficiency is a small sample size from 
# partitioning the data; however, this was not an issue in this situation because 
# we ran the model with age and Gender and it worked. We tried to alter p to
# values between 0.6 and 0.8 and this did not ameliorate the model's ability 
# to run.

# if we delete pregnant and labResults column, the QDA model will be able to run
# since these two columns have high collinearity and the accuracy rate is about 
# 61% for the QDA model if we delet the two columns 

###### KNN: All predictors ######

### Make Model
knnModel <- train(intubated~., data = train, method = "knn", preProcess=c("center","scale"))
knnModel$bestTune
plot(knnModel)
#   k
# 3 9
# Best K is estimated to be 9.

### Make predictions
predKNN <- predict(knnModel, newdata = test)

### Accuracy
confusionMatrix(predKNN, test$intubated)
A <- 35337; B <- 7704; C <- 768; D <- 325

# Prediction      No    Yes
#            No  35337  7704
#           Yes   768   325

# Accuracy rate is 80.8%.
# Error rate is 19.2%.

sensitivity <- A/(A+C); sensitivity
# We predicted positive when it was actually positive 97.9% of the time.

specificity <- D/ (B+D); specificity
# We predicted negative when it was actually negative 4.05% of the time.

Precision <- A/(A+B); Precision
# We predicted positives correctly 82.1% of the time.

NegativePredValue <- D / (C+D); NegativePredValue
# We predicted negatives correctly 29.7% of the time.

Prevalence <- (A+C) /(A+B+C+D); Prevalence
# 81% of the population in the data set are not intubated.

detectionRate <- (A) / (A+B+C+D); detectionRate
# 80% of the population in the data set were correctly identified as being not intubated.

DetectionPrevalence <- (A+B) /(A+B+C+D); DetectionPrevalence
# 97.5% of the population in the data set were identified as being not intubated. 

balanceAccuracy <- (sensitivity + specificity) / 2; balanceAccuracy
# Our balance accuracy indicates that we predicted true positives well, but true 
# negatives we predicted poorly.
# This is evident in our sensitivity of 97.9%, and specificity of 4.05%


###### Best model ######
# We found that LDA and Logsitic regression are the best techniques to use in this 
# case. With essentially identical error rates, the only difference between
# these models is that logistic regression assumes a linear decision boundary. 
# The error rate for our KNN model is 19.2%, which is greater than both logistic
# and LDA. The QDA model was unable to run, likely due to the number of observations
# and variables present. QDA can perform better with a limited number of training
# observations, which is possibly why it was able to run with just age and Gender.



