

Consumer <- read.csv(file.choose())
attach(Consumer)
##Assignment code
Leslie <-read.csv(file.choose())
attach(Leslie)

str(Leslie)
Leslie$County <- factor(Leslie$County,  levels=c("0","1"))
Leslie$Flood <- factor(Leslie$Flood,  levels=c("0","1"))
str(Leslie)
tail(Leslie)
summary(Leslie)
install.packages("GGally")
library(GGally)
ggpairs(Leslie)


##View Structure of Data
str(Consumer)
##This shows the tail of the dataframe... Just in case the table has totals or so in the last row, helps us check the same.
tail(Consumer)

##Clean the data by removing the redundant 4 columns
Consumer$X <- NULL
Consumer$X.1 <- NULL
Consumer$X.2 <- NULL
Consumer$X.3 <- NULL

##Optional step. Incase you have the following packages, please ignore this step
install.packages("psych")
install.packages("lmtest")
install.packages("zoo")

##Load the psych and lmtest Libraries
library(psych)
install.packages('lmtest')
library(lmtest)
library(zoo)
## Summary Statistics

describe(Leslie)
cor(Leslie)
describe(Consumer)
cor(Consumer)
#An additional plot to check for correlation - instead of using the matrix
install.packages("corrplot")
library(corrplot)
corrplot(cor(Consumer))
corrplot(cor(Leslie))

with(Consumer, boxplot(Income, main="Income (1000) US$"))
with(Consumer, boxplot(HouseholdSize, main="Household Size"))
with(Consumer, boxplot(AmountCharged, main="Amount Charged US$"))
describe(Leslie)
with(Leslie, boxplot(Sewer, main="Sewer"))
#Creating a scatter plot between Amount Charged and the independent variable
with(Consumer, plot(HouseholdSize, AmountCharged, pch=19, cex=0.6))
with(Consumer, plot(Income, AmountCharged, pch=19, cex=0.6))


## Simple Regressions with one Independent Variable
reg1 <- lm(AmountCharged ~ Income, data=Consumer)
reg1
summary(reg1)
anova(reg1)
##What is residual standard error in the table below?
##Squareroot of Residual sum of squares divided by its Df
#sqrt(25699404/48)
#R-Square in the total variance explained by the independent variable
#(Sum of squares of the independent variable)/(Total sum of squares)
#(16999745/(16999745+25699404))
#Adjusted R-Square is as follows
##1-(Residual sum sq/its Df)/(Total sum of squares/Total Df)
#1-(25699404/48)/((16999745+25699404)/49)
##http://blog.minitab.com/blog/adventures-in-statistics-2/multiple-regession-analysis-use-adjusted-r-squared-and-predicted-r-squared-to-include-the-correct-number-of-variables



#**********************************************************


reg2 <- lm(AmountCharged ~ HouseholdSize, data=Consumer)
reg2
summary(reg2)
anova(reg2)

#**********************************************************
  
## Multiple Regression with 2 Independent Variables
reg3 <- lm(AmountCharged ~ Income + HouseholdSize,data=Consumer)
reg3
summary(reg3)
anova(reg3)

##Extract the fitted values and residual values from the reg3 output
fitted(reg3)
residuals(reg3)
fit3 <- fitted(reg3)
res3 <- residuals(reg3)


##Merge the fitted and residual values with Consumer dataset for comparison sake
ConsumerReg <- cbind(Consumer, fit3, res3)


##Plot the actual versus fitted values in a plot
with(ConsumerReg, plot(fit3,res3, pch=19, cex=0.6))
abline(a=0,b=0)

# https://stats.stackexchange.com/questions/76226/interpreting-the-residuals-vs-fitted-values-plot-for-verifying-the-assumptions
#Characterestics of a well behaved residual vs Fit plot and their take aways
#The appropriateness of the linear model
#Closer the dots to the line better the model's ability to predict

###*****3 points to look out for
# 1. Residuals seem to bounce randomly aroundly the 0 line causing 
#no set patterns (Typically like cone or U Shaped patterns)
#Suggests that assumption of linear relashionship is within means


#2. Points to form a horizontal band
# This shows that the variance of the error terms are more or less equal
#This shows the lack of violation of Homoscedesticity
#Appropriate for alinear regression model.


#3. Also in the event any points are behaving as outliers, 
# this charts shows it to us.



## Prediction of new observations (Creating a new data frame with a single instance)
newobs <- data.frame(Income = 40, HouseholdSize = 3)
newobs
predict.lm(reg3, newdata=newobs)

# newobs <- data.frame(Income = c(40,50), HouseholdSize = c(3, 4))
# predict.lm(reg3, newdata=newobs)

describe(Leslie)
##Leslie$Elevation,Leslie$Sewer,Leslie$Date,Leslie$Distance
cor(Leslie$Price,Leslie$Size)
cor(Leslie$Price,Leslie$Elevation)
cor(Leslie$Price,Leslie$Sewer)
cor(Leslie$Price,Leslie$Date)
cor(Leslie$Price,Leslie$Distance)
cor(Leslie$Price,Leslie$Flood)


## Model 1:

LesM1 <- lm(Price ~ .,data=Leslie)
LesM1
summary(LesM1)
describe(LesM1)

#Model 2:
LesM2 <- lm(Price ~ .-Distance,data=Leslie)
LesM2
summary(LesM2)

##Extract the fitted values and residual values from the LesM2 output
fitted(LesM2)
residuals(LesM2)
LesFit <- fitted(LesM2)
LesResi <- residuals(LesM2)
##Merge the fitted and residual values with Leslie dataset for comparison sake
LeslieReg <- cbind(Leslie, LesFit, LesResi)


##Plot the actual versus fitted values in a plot
with(LeslieReg, plot(LesFit,LesResi, pch=19, cex=0.6))
abline(a=0,b=0)

## Model: 3
LesM3 <- lm(Price ~ .-Distance,data=Leslie[-26,])
LesM3
summary(LesM3)
fitted(LesM3)
residuals(LesM3)
LesFit1 <- fitted(LesM3)
LesResi1 <- residuals(LesM3)
##Merge the fitted and residual values with Leslie dataset for comparison sake
LeslieReg1 <- cbind(Leslie[-26,], LesFit1, LesResi1)


##Plot the actual versus fitted values in a plot
with(LeslieReg1, plot(LesFit1,LesResi1, pch=19, cex=0.6))
abline(a=0,b=0)


##Model 4

## Model: 3
LesM4 <- lm(Price ~ .-Distance-Sewer,data=Leslie[-26,])
LesM4
summary(LesM4)

## Model : 5
LesM5 <- lm(Price ~ .-Distance-Sewer-Size,data=Leslie[-26,])
LesM5
summary(LesM5)
##Assumptions
#Model 6:
LesM6 <- lm(Price ~ .-Size,data=Leslie)
LesM6
summary(LesM6)

##Linear relationship
with(Leslie, plot(Price, Size, pch=19, cex=0.6))
with(Leslie, plot(Elevation, Price, pch=19, cex=0.6))



##Multivariate normality Test
with(Consumer, shapiro.test(Income))
#Requires additional data / transformation etc
with(Consumer, shapiro.test(HouseholdSize))

with(Consumer, qqnorm(Income, pch=19, cex=0.6))
with(Consumer, qqline(Income, col='red'))

with(Consumer, qqnorm(HouseholdSize, pch=19, cex=0.6))
with(Consumer, qqline(HouseholdSize, col='red'))

#Residuals on a QQplot
with(ConsumerReg, qqnorm(res3, pch=19, cex=0.6))
with(ConsumerReg, qqline(res3, col='red'))



##No or little multicollinearity - Correlation or VIF variation inflation factor test
cor(Consumer)
#Install corrplot incase you do not have this package
install.packages("corrplot")
library(corrplot)
corrplot(cor(Consumer))
#VIF - Variation inflation factor -as a thumb rule if the VIF is greater than 10 it suggests there is multi collinerity
# If the VIF is greater than 100 it certainly demands us to fix this as it could greatly affect results in regression
library(car)
vif(reg3)
vif(LesM3)

#What is Auto Correlation  https://www.cengage.com/resource_uploads/downloads/0030348064_54176.pdf
#Durbin Watson Test to test Auto Correlation
#DW ranges between 0 and 4. a Value of 2 indicates there is no Auto correlation.
#0 indicated positive auto correlation. 4 indicates negative auto correlation
#Thumb rule 1.5 to 2.5 safe to conclude no Auto correlation
##Null Hypothesis states that there is No auto-correlation 
#Alternate Hypothesis states there is autocorrelation
dwtest(reg3)
dwtest(LesM3)


## Homoscedasticity tested using Goldfelt Quant test
#Null hypothesis : Data satisfies the condiction of homoscedasticity
##Alternate hypothesis states data is not Homoscedastic
gqtest(reg3)
gqtest(LesM3)

##***************************************************************************
 
##Group assignment : Problem 4

setwd("C:/Users/Lakshmi/Desktop/R programming")
getwd()

AGF <- read.csv(file.choose())
attach(AGF)
describe(AGF)
summary(AGF)
ggpairs(AGF)
corrplot(cor(AGF))
## Model : 1
AGF1 <- lm(X1 ~ .,data=AGF)
AGF1
summary(AGF1)
vif(AGF1)
dwtest(AGF1)
gqtest(AGF1)

## Model : 2
AGF2 <- lm(X1 ~ .-X3,data=AGF)
AGF2
summary(AGF2)
vif(AGF2)
cor(AGF$X1,AGF$X6)
dwtest(AGF2)
gqtest(AGF1)
