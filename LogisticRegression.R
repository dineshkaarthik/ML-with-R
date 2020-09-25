

## Six steps of Logistic Regression by Paul Green

mydata = read.csv("nm-logit.csv",header = TRUE)
attach(mydata)
names(mydata)

## Step 1: Log Likelyhood test
logit = glm(Loyalty~Brand+Product+Shopping,data = mydata, family = binomial)
library(lmtest)
lrtest(logit)

## Step 2: Pseudo R square  -- Consider the McFadden value for R square 
## McFadden R square is G2/11hNull

install.packages("pscl")
library(pscl)
pR2(logit)

# 43.53% of uncertainity produced by intercept model is caliberated by full model

## less than 10% is poor fit(modle veracity can be questioned) and between than 10% to 30% then model is satisfactory and 30% to 50% then model is good and above 50% is excellent

## Step 3: Individual Betas: are they significant?

summary(logit)

## Step 4: Explanatory power of odds and probablity

exp(coef(logit))

## if odds ratios are more than 1 then it is the best combination though some are statistically insignificant.

## Step 5: Classification occuracy: Confusion matrix or misclassification error

prediction = predict(logit,type="response")
cutoff = floor(prediction+0.5)
table(Actual = mydata$Loyalty,Predicted = cutoff)

## Step 6: ROC(Receiver operating characteristic Curve) curve: 
install.packages("rjava")
install.packages("Deducer")
library(Deducer)
rocplot(logit)
## 90% is excellent, 80 to 90% is good, below 70% is not good


##Example : In class assignment
mytrain = read.csv("PaulBooks1.csv",header = TRUE)
attach(mytrain)
names(mytrain)
paulbooks = glm(Purchase~Months+NoBought, data = mytrain,family = binomial)
summary(paulbooks)
lrtest(paulbooks)

## Step2
pR2(paulbooks)

##Step 3:

summary(paulbooks)

## Step 4:

exp(coef(paulbooks))

##Step 5
prediction1 = predict(paulbooks,type="response")
cutoff = floor(prediction1+0.5)
table(Actual = mytrain$Purchase,Predicted = cutoff)

## Step 6

rocplot(paulbooks)

## validate with test data

mytest = read.csv("PaulBooks2.csv",header = TRUE)
attach(mytest)
PredictionTest = predict(paulbooks,type="response",newdata = mytest)
cutoffTest = floor(PredictionTest+0.5)
table(Actual=mytest$Purchase,PredictionTest)

##--------------------------------------------------------------------

logit = glm(Loyalty~Brand+Product+Shopping, data = mydata,family = binomial)
summary(logit)
library(lmtest)
lrtest(logit)


