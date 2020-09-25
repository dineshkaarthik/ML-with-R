#load libraries and data
#install.packages("mlr")
library(caret)
library(mlr)
train_mv = read.csv("03_loan_Complete_train.csv", na.strings = c(""," ",NA))
test_mv = read.csv("03_loan_Complete_test.csv", na.strings = c(""," ",NA))

summarizeColumns(train_mv)

train_mv$Credit_History= as.factor(train_mv$Credit_History)
test_mv$Credit_History= as.factor(test_mv$Credit_History)

table(train_mv$Dependents)

levels(train_mv$Dependents)[4] = "3"
levels(test_mv$Dependents)[4] = "3"

table(train_mv$Dependents)

#impute missing values by mean and mode *****NEW*****
imp = impute(train_mv, classes = list(factor = imputeMode(), integer = imputeMean()), dummy.classes = c("integer","factor"), dummy.type = "factor")
imp1 = impute(test_mv, classes = list(factor = imputeMode(), integer = imputeMean()), dummy.classes = c("integer","factor"), dummy.type = "factor")

train = imp$data
test = imp1$data

train$Married.dummy=NULL
train$Loan_Amount_Term.dummy=NULL

summarizeColumns(train)
summarizeColumns(test)


## Capping outliers

train = capLargeValues(train, target = "Loan_Status",cols = c("ApplicantIncome"),threshold = 40000)
train = capLargeValues(train, target = "Loan_Status",cols = c("CoapplicantIncome"),threshold = 21000)
train= capLargeValues(train, target = "Loan_Status",cols = c("LoanAmount"),threshold = 520)





test = capLargeValues(test, target = "Loan_Status",cols = c("ApplicantIncome"),threshold = 33000)
test = capLargeValues(test, target = "Loan_Status",cols = c("CoapplicantIncome"),threshold = 16000)
test = capLargeValues(test, target = "Loan_Status",cols = c("LoanAmount"),threshold = 470)



#***** NEW*****Introduction to Feature Engineering


#Total_Income
train$Total_Income = train$ApplicantIncome + train$CoapplicantIncome
test$Total_Income = test$ApplicantIncome + test$CoapplicantIncome

#Income by loan
train$Income_by_loan = train$Total_Income/train$LoanAmount
test$Income_by_loan = test$Total_Income/test$LoanAmount

#change variable class
#train$Loan_Amount_Term =  as.numeric(train$Loan_Amount_Term)
#test$Loan_Amount_Term = as.numeric(test$Loan_Amount_Term)

#Loan amount by term
train$Loan_amount_by_term = train$LoanAmount/train$Loan_Amount_Term
test$Loan_amount_by_term = test$LoanAmount/test$Loan_Amount_Term


cor(train$ApplicantIncome,train$Total_Income)
cor(train$ApplicantIncome,train$Income_by_loan)
cor(train$ApplicantIncome,train$Loan_amount_by_term)

## In a generic way, it is better to check with corrplot

library(dplyr)
forCorr=select_if(train, is.numeric)
cor=cor(forCorr)
library(corrplot)

corrplot(cor, type="lower")





train$Total_Income = NULL
test$Total_Income = NULL


summarizeColumns(train)
summarizeColumns(test)

#### From here starts the normal process


trainTask = makeClassifTask(data = train,target = "Loan_Status", positive = "Y")
testTask = makeClassifTask(data = test, target = "Loan_Status", positive = "Y")

trainTask

#normalize the variables***** NEW*****
trainTask = normalizeFeatures(trainTask,method = "standardize")
testTask = normalizeFeatures(testTask,method = "standardize")

trainTask = dropFeatures(task = trainTask,features = c("Loan_ID"))

#Load NB
nb.learner=makeLearner("classif.naiveBayes",predict.type="response")
nb.model=train(nb.learner,trainTask)
nb.predict=predict(nb.model,testTask)

confusionMatrix(nb.predict$data$truth,nb.predict$data$response, positive = "Y")


#load qda 
qda.learner = makeLearner("classif.qda", predict.type = "response")

qda.model = train(qda.learner, trainTask)

qda.predict = predict(qda.model, testTask)

confusionMatrix(qda.predict$data$truth,qda.predict$data$response, positive = "Y")


## Logistic Regression

logistic.learner=makeLearner("classif.logreg", predict.type = "response")
logistic.model=train(logistic.learner, trainTask)
logistic.predict= predict(logistic.model, testTask)
confusionMatrix(logistic.predict$data$truth, logistic.predict$data$response, positive = "Y")

## CART

cart.learner=makeLearner("classif.rpart", predict.type = "response")
cart.model=train(cart.learner, trainTask)
cart.predict= predict(cart.model, testTask)
confusionMatrix(cart.predict$data$truth, cart.predict$data$response, positive = "Y")

## RandomForest

rf.learner=makeLearner("classif.randomForest", predict.type = "response")
rf.model=train(rf.learner, trainTask)
rf.predict= predict(rf.model, testTask)
confusionMatrix(rf.predict$data$truth, rf.predict$data$response, positive = "Y")



## SVM
install.packages("kernlab")
library(kernlab)
svm.learner=makeLearner("classif.ksvm", predict.type = "response")
svm.model=train(svm.learner, trainTask)
svm.predict= predict(svm.model, testTask)
confusionMatrix(svm.predict$data$truth, svm.predict$data$response, positive = "Y")

## GBM
install.packages("gbm")
library(gbm)
gbm.learner=makeLearner("classif.gbm", predict.type = "response")
gbm.model=train(gbm.learner, trainTask)
gbm.predict= predict(gbm.model, testTask)
confusionMatrix(gbm.predict$data$truth, gbm.predict$data$response, positive = "Y")


## Incorporating Cross Validation


cv.logistic = crossval(learner = logistic.learner,task = trainTask,iters = 3,stratify = TRUE,measures = acc,show.info = T)

#Getting the accuracy

cv.logistic$aggr
cv.logistic$measures.test



## Tuning CART model with Cross Validation
## List of available parameters to tune

getParamSet(cart.learner)


## Set Resampling Strategy

set_cv = makeResampleDesc("CV",iters = 3L)

#Search for hyperparameters
gs = makeParamSet(
  makeIntegerParam("minsplit",lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = 0.001, upper = 0.2)
)

gscontrol = makeTuneControlGrid()

#Tuning Process

stune = tuneParams(learner = cart.learner, resampling = set_cv, task = trainTask, par.set = gs, control = gscontrol, measures = acc)

stune$x
stune$y

tuned.learner = setHyperPars(cart.learner, par.vals = stune$x)
tuned.rpartModel = train(tuned.learner, trainTask)


#make predictions
cart.tunedpredict = predict(tuned.rpartModel, testTask)
confusionMatrix(cart.tunedpredict$data$truth, cart.tunedpredict$data$response, positive = "Y")

## Random Forest

getParamSet("classif.randomForest")

## Tuniing of parameters

rf_param = makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)


rancontrol = makeTuneControlRandom(maxit = 50L)
set_cv = makeResampleDesc("CV",iters = 3L)
tuned.rf = tuneParams(learner = rf.learner, resampling = set_cv, task = trainTask, par.set = rf_param, control = rancontrol, measures = acc)

tuned.rfLearner = setHyperPars(rf.learner, par.vals = tuned.rf$x)
tuned.rfModel = train(tuned.rfLearner, trainTask)
tuned.rfPredict = predict(tuned.rfModel, testTask)


confusionMatrix(tuned.rfPredict$data$truth, tuned.rfPredict$data$response, positive = "Y")


#load svm
getParamSet("classif.ksvm") 


#Set parameters
pssvm = makeParamSet(
  makeDiscreteParam("C", values = 2^c(-8,-4,-2,0)), #cost parameters
  makeDiscreteParam("sigma", values = 2^c(-8,-4,0,4)) #RBF Kernel Parameter
)

#specify search function
ctrl = makeTuneControlGrid()

#tune model
svm.tuned = tuneParams(svm.learner, task = trainTask, resampling = set_cv, par.set = pssvm, control = ctrl,measures = acc)

#CV accuracy
svm.tuned$x


#set the model with best params
tuned.SVMLearner = setHyperPars(svm.learner, par.vals = svm.tuned$x)

#train
tuned.svm = train(tuned.SVMLearner, trainTask)

#test
tuned.SVMPredict = predict(tuned.svm, testTask)

confusionMatrix(tuned.SVMPredict$data$truth, tuned.SVMPredict$data$response, positive = "Y")

#load GBM
getParamSet("classif.gbm")


#specify tuning method
rancontrol = makeTuneControlRandom(maxit = 50L)

#3 fold cross validation
set_cv = makeResampleDesc("CV",iters = 3L)

#parameters
gbm_par = makeParamSet(
  makeDiscreteParam("distribution", values = "bernoulli"),
  makeIntegerParam("n.trees", lower = 100, upper = 1000), #number of trees
  makeIntegerParam("interaction.depth", lower = 2, upper = 10), #depth of tree
  makeIntegerParam("n.minobsinnode", lower = 10, upper = 80),
  makeNumericParam("shrinkage",lower = 0.01, upper = 1)
)

tune_gbm = tuneParams(learner = gbm.learner, task = trainTask,resampling = set_cv,measures = acc,par.set = gbm_par,control = rancontrol)

#check CV accuracy
tune_gbm$y

#set parameters
final_gbm = setHyperPars(learner = gbm.learner, par.vals = tune_gbm$x)

#train
to.gbm = train(final_gbm, trainTask)

#test 
pr.gbm = predict(to.gbm, testTask)

confusionMatrix(pr.gbm$data$truth,pr.gbm$data$response, positive = "Y")

## ROC and AUC (Only for 2 Algorithms)

logistic.learner2=makeLearner("classif.logreg", predict.type="prob")
logistic.model2=train(logistic.learner2,trainTask)
logistic.predict2=predict(logistic.model2,testTask, type="prob")
df = generateThreshVsPerfData(logistic.predict2, measures = list(fpr, tpr, mmce))
plotROCCurves(df)
performance(logistic.predict2,auc)
plotThreshVsPerf(df)


svm.learner2= makeLearner("classif.ksvm", predict.type = "prob")
svm.model2=train(svm.learner2, trainTask)
svm.predict2=predict(svm.model2, testTask)
df2 = generateThreshVsPerfData(svm.predict2, measures = list(fpr, tpr, mmce))
plotROCCurves(df2)
performance(svm.predict2,auc)
plotThreshVsPerf(df2)


## LASSO and RIDGE Regression

glmnet.learner=makeLearner("classif.glmnet", alpha= 1, predict.type = "response")
glmnet.modelLasso= train(glmnet.learner, trainTask)

glmnetLassoModel= getLearnerModel(glmnet.modelLasso)

plot(glmnetLassoModel, "norm", label = TRUE)
plot(glmnetLassoModel, "lambda", label = TRUE)
plot(glmnetLassoModel, "dev", label = TRUE)


glmnet.learner2=makeLearner("classif.glmnet", alpha= 0, predict.type = "response")
glmnet.modelRidge= train(glmnet.learner2, trainTask)

glmnetRidgeModel= getLearnerModel(glmnet.modelRidge)

plot(glmnetRidgeModel, "norm", label = TRUE)
plot(glmnetRidgeModel, "lambda", label = TRUE)
plot(glmnetRidgeModel, "dev", label = TRUE)
