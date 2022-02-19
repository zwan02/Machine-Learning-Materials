#Case Study 2
#Team 22 - Fares Sukkar, Spencer Tsai, Frank Wan, Zach Washburn

#set working directory
rm(list=ls())

setwd("C:/Users/Fares/Desktop/MSBA/ML I/Case Study/Case Study 2")
Case2 <- read.csv("Case2.csv") 
str(Case2)
summary(Case2)

#Tuition and Fees Linear Regression code - Frank

lmCase2B <- lm(FTRetentionRate~TuitionAndFees, data = Case2)
plot(FTRetentionRate~TuitionAndFees, data = Case2)
summary(lmCase2B)
abline(lmCase2B,col='red')
plot(lmCase2B)
hist(lmCase2B$residuals) 
mean(lmCase2B$residuals)
lmtest::bptest(lmCase2B)  

#Tuition and Fees Analysis

#TuitionAndFees is a significant predictor of FTRetentionRate since it has
#a p-value less than 0.05. However,this is a weak relationship because 
#the R-squared and Adjusted-R Squared values (0.09602 and 0.09546, respectively)
#explain 9.6% of the variance in FTRetentionRate. 

#As a result, the entire model is weakly significant.

#From the plot of TuitionAndFees and FTRetentionRate, we can tell that the 
#relationship between these two variables is slightly positive, indicated by a 
#relatively flat slope from the plot.

#There are three outliers indexed at 14,171,1041 that have the most errors. 
#Also, in the QQ plot, there are some outliers in the lower left side of the plot.
#The histogram of residuals is slightly left skewed and the mean residuals is 
#close to 0. We found that the p-value is less than 0.05, which violates the
#the assumption of homoskedasticity. This means that homoskedasticity will not 
#cause bias in the coefficient estimates but it does make them less precise.

#Based on the findings of this model and given that it is a weak relationship
#between tuition and retention rates, higher tuition fees have a positive 
#relationship with retention rates, though it does not fully justify that it

#Although tuition and fees is a significant predictor, it has a weak, positive 
#relationship, so it may not accurately determine retention rates. For example,
#states schools like Virginia Tech and the University of Virginia have relatively
#low tuition fees and maintain high retention rates while schools with high
#tuition fees, like Yale and Columbia, also maintain a high retention rate. 
#This is significant because it shows that choosing a school based on price, 
#will not likely determine whether a student will remain after a year.


#Average Salary Linear Regression code - Fares

lmCase2C <- lm(FTRetentionRate~AverageSalary, data = Case2) 
summary(lmCase2C)
plot(FTRetentionRate~AverageSalary, data = Case2)
abline(lmCase2C,col='red')
plot(lmCase2C)
hist(lmCase2C$residuals)
mean(lmCase2C$residuals)
lmtest::bptest(lmCase2C)

#Average Salary Analysis

#FTRetentionRate and AverageSalary have a small, positive
#relationship as the coefficient for AverageSalary is 5.473e+01 and
#has a statistical significance with a p-value of 2.2e-16.]

#Variability in average salary between $50,000 and $100,000 
#appears to be relatively high but narrows as average salary 
#increases. However, given the R-squared and Adjusted R-squared 
#values are approximately 0.26, indicates that average salary explains more 
#variation than tuition and fees. 

#Plotting the histogram of the residuals, we see that the 
#data is close to a mean of 0, with a slight left skew, which 
#indicates there is bias in our model. #As for the result of the BP test, 
#we have a p-value of 2.458e-11, indicating it violates the assumption of 
#homoskedasticity.

#As the Average Salary increases, it appears that retention rate also increases.
#As previously stated, this displays a  relatively weak but positive 
#relationship, confirmed through  the Intercept. For example, a handful of the
#data points with high salaries and high retention rates are Ivy League schools,
#such as Yale, Harvard, and Dartmouth, where the most qualified academics 
#likely work and earn a premium salary for their research. The rigor of these 
#schools' academic programs likely draws many high performing students who seek 
#to learn from these professors.


#Student to Faculty (S2F) Ratio Linear Regression code - Spencer

lmCase2F <- lm(FTRetentionRate~S2FRatio, data = Case2)
summary(lmCase2F)
plot(FTRetentionRate~S2FRatio, data = Case2)
abline(lmCase2F,col='red')
plot(lmCase2F)
hist(lmCase2F$residuals)
mean(lmCase2F$residuals)
lmtest::bptest(lmCase2F)

#S2F Ratio Analysis

#FTRetentionRate and S2FRatio have a small, negative relationship as the 
#coeffecient for S2FRatio is -0.30678. S2FRatio is statistically significant 
#with a p-value of 1.93e-06. Residuals and standard residuals fit randomly about
#the fitted values.

#The histogram of the residuals is slightly skewed left. This implies a small 
#amount of bias in our model.

#The mean of the residuals is zero. BP test yields a p-value of 5.971e-09, 
#indicating that it violates the assumption of homoskedasticity.

#Based on the findings of this model, a higher S2F ratio indicates a lower 
#retention rate. This could be attributed to lack of meaningful connections 
#between students and professors. A smaller ratio allows for more direct
#and interpersonal communication, which could enable a healthier learning 
#environment.