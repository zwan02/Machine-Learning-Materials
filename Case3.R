###################################
######### Team 22, Case 3 #########
### Fares Sukkar, Spencer Tsai, ###
#### Frank Wan, Zach Washburn #####
###################################

rm(list = ls())

library(lmtest)
library(car)

Case3 <- read.csv("Case3.csv", stringsAsFactors = T)
attach(Case3)
str(Case3)
summary(Case3)

lmInitial <- lm(SalePrice~LotFrontage+OverallQual+Fireplaces,data=Case3)
plot(lmInitial)
#the initial model we have is relatively linear as confirmed by the residuals vs. fitted plot

summary(lmInitial)
# our initial model is explaining about 70.4% of the variance. Also, the entire 
#model and all the independent variable coefficients are significant because the 
#p-value is significantly below 0.05

hist(lmInitial$residuals)
mean(lmInitial$residuals)
#the residual data does have a positive skew, but the mean of the error terms 
#is very close to 0 which means the error has the normal distribution

lmtest::bptest(lmInitial)
#the result of our bp-test for the initial model reveals that our p-value is below 
#0.05 which violates the assumption of homoscedasticity

car::vif(lmInitial)
#the vif values for each independent variables in the initial model is all below 5
#which means this model is not multicollinear

cor(Case3[c("LotFrontage","OverallQual","Fireplaces")])

Case3 <- Case3[-c(430,1062,1095),]
## Initial plots of transformed model had some obvious outliers. Indexed observation 1062 is a glaring
## issue, where it tails off from the other variables in the Q-Q plot. After omitting this observation,
## we found that observations 430 and 1095 presented similar issues. The model improved after removal
## of these outliers.

lmFinal <- lm(log(SalePrice)~LotFrontage+OverallQual+Fireplaces,data=Case3)
plot(lmFinal)
## The final plots reveals that residuals appear random about the mean. There is minimal deviation 
## from the Q-Q plot.

options(scipen = 999)
summary(lmFinal)
## Model is significant, with an adjusted R-Square value of 0.7602. This means that our model
## explains about 76% of the variance. Each independent variable and the entire model are significant,
## with p-values near zero. 
## Interpretation of the independent variable coefficients is as follow:
## When Lotfrontage increases 1 foot, the SalePrice of the home increases 0.307%.
## When OverallQual increases 1 rating, the SalePrice of the home increases 21.43%.
## When one additional Fireplace is added, there is a 9.39% increase in SalePrice.

hist(lmFinal$residuals)
## Histogram of residuals appears normally distributed. 

lmtest::bptest(lmFinal)
## P-Value of BP test is 0.6192, well over the 0.05 cutoff. Our model is homoskedastic.

car::vif(lmFinal)
## All VIF scores under the threshold of 5.

cor(Case3[c("LotFrontage","OverallQual","Fireplaces")])
## A correlation matrix of the independent variables reveals little collinearity.

Sig <- sigma(lmFinal); Sig 
b0 <- lmFinal$coefficients[1]; b0
b1 <- lmFinal$coefficients[2]; b1
b2 <- lmFinal$coefficients[3]; b2
b3 <- lmFinal$coefficients[4]; b3
#according to the new model we have, we checked the intercept which is b0 and independent 
#variable coefficients which are b1, b2 and b3

#final correlation
yhat <- exp(b0 + b1*Case3$LotFrontage+b2*Case3$OverallQual+b3*Case3$Fireplaces)
#In the final step, we formulated the final linear regression equation for the new model
cor(yhat, SalePrice)^2

