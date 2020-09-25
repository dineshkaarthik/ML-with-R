
library(caret)
library(mlr)
##Read the data

airline = read.csv("ws_00_AirlineDelay")
airlinePart1 = airline
##Split the data into testing and training
library(caTools)
set.seed(144)
spl = sample.split(airlinePart1$TotalDelay, SplitRatio = 0.7)
part1Train = subset(airlinePart1, spl == TRUE)
part1Test = subset(airlinePart1, spl == FALSE)


## Linear Regression Model

lrTrainTask= makeRegrTask(data=part1Train, target = "TotalDelay")
lrTestTask=makeRegrTask(data=part1Test, target = "TotalDelay")

lr.learner= makeLearner("regr.lm")
lr.model=train(lr.learner, lrTrainTask)

lr.model=getLearnerModel(lr.model)
summary(lr.model)


## Corrplot

library(dplyr)
forCorr=select_if(part1Train, is.numeric)
cor=cor(forCorr)
library(corrplot)

corrplot(cor, type="lower")


## Predictions

lr.predict=predict(lr.model, part1Test)

sse= sum((lr.predict-part1Test$TotalDelay)^2)


## LASSO Regression

glmnet.learnerLASSO=makeLearner("regr.glmnet", alpha= 1, predict.type = "response")
glmnet.modelLasso= train(glmnet.learnerLASSO, lrTrainTask)

glmnetLassoModel= getLearnerModel(glmnet.modelLasso)

#install.packages("plotmo")
library(plotmo) # for plot_glmnet
plot_glmnet(glmnetLassoModel)                             # default colors
plot_glmnet(glmnetLassoModel, label=5)

## RIDGE REGRESSION

glmnet.learnerRIDGE=makeLearner("regr.glmnet", alpha= 0, predict.type = "response")
glmnet.modelRIDGE= train(glmnet.learnerRIDGE, lrTrainTask)

glmnetRIDGEModel= getLearnerModel(glmnet.modelRIDGE)

plot_glmnet(glmnetRIDGEModel)                             # default colors
plot_glmnet(glmnetRIDGEModel, label=5)


## Multiclass CART

airlinePart2=airline

airlinePart2$DelayClass = factor(ifelse(airlinePart2$TotalDelay == 0, "No Delay", ifelse(airlinePart2$TotalDelay >= 30, "Major Delay", "Minor Delay")))
airlinePart2$TotalDelay=NULL

table(airlinePart2$DelayClass)

part2TrainTask= makeClassifTask(data=airlinePart2, target = "DelayClass")
part2Learner= makeLearner("classif.rpart", predict.type = "response")
part2Model= train(part2Learner, part2TrainTask)

part2Model1= getLearnerModel(part2Model)
prp(part2Model1, extra=2, roundint = FALSE)





##Model Comparisons

airlinePart3= airline

airlinePart3$DelayClass=factor(ifelse(airlinePart3$TotalDelay==0, "No Delay", "Delay"))

airlinePart3$TotalDelay=NULL

## Splitting Data


set.seed(144)
spl2 = sample.split(airlinePart3$DelayClass, SplitRatio = 0.7)
part3Train = subset(airlinePart3, spl == TRUE)
part3Test = subset(airlinePart3, spl == FALSE)


## Making Train and Test Task

part3TrainTask= makeClassifTask(data=part3Train, target = "DelayClass", positive = "Delay")
part3TestTask= makeClassifTask(data=part3Test, target = "DelayClass", positive = "Delay")


## Logistic Regression

logistic.part3= makeLearner("classif.logreg", predict.type = "response")
logistic.part3Model=train(logistic.part3, part3TrainTask)
logistic.part3Predict=predict(logistic.part3Model, part3TestTask)

confusionMatrix(logistic.part3Predict$data$truth, logistic.part3Predict$data$response, positive = "Delay")


## SVM

svm.part3= makeLearner("classif.ksvm", predict.type = "response")
svm.part3Model=train(svm.part3, part3TrainTask)
svm.part3Predict=predict(svm.part3Model, part3TestTask)

confusionMatrix(svm.part3Predict$data$truth, svm.part3Predict$data$response, positive = "Delay")


## GBM

## Logistic Regression

gbm.part3= makeLearner("classif.gbm", predict.type = "response")
gbm.part3Model=train(gbm.part3, part3TrainTask)
gbm.part3Predict=predict(gbm.part3Model, part3TestTask)

confusionMatrix(gbm.part3Predict$data$truth, gbm.part3Predict$data$response, positive = "Delay")


## Random Forest

rf.part3= makeLearner("classif.randomForest", predict.type = "response")
rf.part3Model=train(rf.part3, part3TrainTask)
rf.part3Predict=predict(rf.part3Model, part3TestTask)

confusionMatrix(rf.part3Predict$data$truth, rf.part3Predict$data$response, positive = "Delay")


## Tuning Random Forest

rf_param = makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)


rancontrol = makeTuneControlRandom(maxit = 50L)
set_cv = makeResampleDesc("CV",iters = 3L)
tuned.rf = tuneParams(learner = rf.part3, resampling = set_cv, task = part3TrainTask, par.set = rf_param, control = rancontrol, measures = acc)

tuned.rfLearner = setHyperPars(rf.part3, par.vals = tuned.rf$x)
tuned.rfModel = train(tuned.rfLearner, part3TrainTask)
tuned.rfPredict = predict(tuned.rfModel, part3TestTask)


confusionMatrix(tuned.rfPredict$data$truth, tuned.rfPredict$data$response, positive = "Delay")



## ROC and AUC

rf.learner2= makeLearner("classif.randomForest", predict.type = "prob")
rf.model2=train(rf.learner2, part3TrainTask)
rf.predict2=predict(rf.model2, part3TestTask)
df2 = generateThreshVsPerfData(rf.predict2, measures = list(fpr, tpr, mmce))
plotROCCurves(df2)
performance(rf.predict2,auc)
plotThreshVsPerf(df2)

